 (* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86


(* allocated llvmlite function bodies --------------------------------------- *)

(* Generating X86 assembly is tricky, and it helps to split the problem into
   two parts: 

   1) Figuring out how to represent states of the LLVMlite machine as
      those of the X86lite machine, i.e. where should we store uid %x
      or global @foo, and

   2) Choosing the X86 instructions that will correspond to LLVMlite
      instructions while maintaining this relationship: For example,
      what sequence of X86 instructions will implement the
      "getelementptr" instruction, assuming that we know the arguments
      are in such and such X86 registers?

   To do this, we will introduce a slightly different representation
   of LLVMlite, where uids, globals, and labels have been replaced
   with their X86 counterparts.  Uids can be mapped onto X86
   registers, stack slots or, for instructions like "store" that do
   not assign to their uid, no storage. Additionally, since in
   LLVMlite labels and uids share a namespace, some uids correspond to
   code labels.

   Rather than working directly with the LLVMlite AST, we will be
   using a flattened "stream of instructions" representation like the
   one you saw in class. Once we have decided how we want to represent
   LLVMlite states in X86, we can convert programs to this
   representation. Then, we will have to choose X86 instructions to
   correspond to the "allocated" LLVMlite instruction stream.
*)
module Alloc = struct

(* X86 locations *)
type loc =
  | LVoid                       (* no storage *)
  | LReg of X86.reg             (* x86 register *)
  | LStk of int                 (* a stack offset from %rbp *)
  | LLbl of X86.lbl             (* an assembler label *)

type operand = 
  | Null
  | Const of int64
  | Gid of X86.lbl
  | Loc of loc

type insn =
  | ILbl
  | Binop of bop * ty * operand * operand
  | Alloca of ty
  | Load of ty * operand
  | Store of ty * operand * operand
  | Icmp of Ll.cnd * ty * operand * operand
  | Call of ty * operand * (ty * operand) list
  | Bitcast of ty * operand * ty
  | Gep of ty * operand * operand list
  | Ret of ty * operand option
  | Br of loc
  | Cbr of operand * loc * loc

(* An allocated function body is just a flattened list of instructions,
   labels, and terminators. All uids, labels, and gids are replaced with the
   associated parts of the x86 machine *)
type fbody = (loc * insn) list

(* Converting between Ll function bodies and allocate function bodies given
   two functions
   f : uid -> loc
   g : gid -> X86.lbl *)
let map_operand f g : Ll.operand -> operand = (* Printf.printf "map_operand\n"; *) function
  | Null -> (* Printf.printf "Null\n";   *)Null
  | Const i -> (* Printf.printf "Const\n"; *) Const i
  | Gid x -> (* Printf.printf "Gid\n"; *) Gid (g x)
  | Id u -> (* Printf.printf "Id %s\n" *) Loc (f u)

let map_insn f g : Ll.insn -> insn = 
  let mo = map_operand f g in 
  (* Printf.printf "map_insn\n"; *) function
  | Binop (b,t,o,o') -> Binop (b,t,mo o,mo o')
  | Alloca t         -> Alloca t
  | Load (t,o)       -> Load (t,mo o)
  | Store (t,o,o')   -> Store (t,mo o,mo o')
  | Icmp (c,t,o,o')  -> Icmp (c,t,mo o,mo o')
  | Call (t,o,args)  -> Call (t,mo o,List.map (fun (t,o) -> t, mo o) args)
  | Bitcast (t,o,t') -> Bitcast (t,mo o,t')
  | Gep (t,o,is)     -> Gep (t,mo o,List.map mo is)

let map_terminator f g : Ll.terminator -> insn = 
  let mo = map_operand f g in (* Printf.printf "map_terminator\n";  *)function
  | Ret (t,None)   -> Ret (t, None)
  | Ret (t,Some o) -> Ret (t, Some (mo o))
  | Br l           -> Br (f l)
  | Cbr (o,l,l')   -> Cbr (mo o,f l,f l')

let of_block f g (b:Ll.block) : fbody =
  let b = List.map (fun (u,i) -> f u, map_insn f g i) b.insns
  @ [LVoid, map_terminator f g b.terminator] in
  (* Printf.printf "Called this"; *) b
                                
let of_lbl_block f g (l,b:Ll.lbl * Ll.block) : fbody =
  (LLbl (Platform.mangle l), ILbl)::of_block f g b

let of_cfg f g (e,bs:Ll.cfg) : fbody =
  List.(flatten @@ of_block f g e :: map (of_lbl_block f g) bs)

end

(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid that is assigned
   a value.  However, since there are an unlimited number of %uids and
   only 16 registers, doing so effectively is quite difficult. We will 
   see later in the course how _register allocation_ algorithms can do a 
   good job at this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack). Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'. A stack layout maps a uid to an Alloc.loc that represents
   where it will be stored. Recall that some uids identify instructions that
   do not assign a value, whereas others name code blocks. These are mapped to 
   Alloc.LVoid, and Alloc.LLbl, respectively. For this compilation strategy, 
   uids that are assigned values will always be assigned an offset from ebp
   (in bytes) that corresponds to a storage slot in the stack.  
*)
type layout = (uid * Alloc.loc) list 

type tdc = (tid * ty) list

(* Once we have a layout, it's simple to generate the allocated version of our
   LLVMlite program *)
let alloc_cfg (layout:layout) (g:Ll.cfg) : Alloc.fbody =
  Alloc.of_cfg (fun x -> (* Printf.printf "Layout\n";  *)List.assoc x layout) 
               (fun l -> Platform.mangle l) g

(* streams of x86 instructions ---------------------------------------------- *)

type x86elt = 
  | I of X86.ins
  | L of (X86.lbl * bool)

type x86stream = x86elt list 

let lift : X86.ins list -> x86stream =
  List.rev_map (fun i -> I i)

let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x

let prog_of_x86stream : x86stream -> X86.prog =
  let rec loop p iis = function
    | [] -> (match iis with [] -> p | _ -> failwith "stream has no initial label")
    | (I i)::s' -> loop p (i::iis) s'
    | (L (l,global))::s' -> loop ({ lbl=l; global; asm=Text iis }::p) [] s'
  in loop [] []


(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

   You might find it useful to implement the following helper function, 
   whose job is to generate the X86 operand corresponding to an allocated 
   LLVMlite operand.
 *)

let compile_operand_base (b:X86.reg) : Alloc.operand -> X86.operand = function 
  | Alloc.Null -> Imm (Lit 0L) 
  | Alloc.Const c -> Imm (Lit c) 
  | Alloc.Gid l -> Ind3 (Lbl l, b)
  | Alloc.Loc l -> 
    begin match l with 
    | Alloc.LReg r -> Reg r
    | Alloc.LStk s -> Ind3 (Lit (Int64.of_int s), b)
    | Alloc.LLbl lb -> Imm (Lbl (Platform.mangle lb))
    | _ -> failwith "Cannot use this as an operand"
    end

let compile_operand o = compile_operand_base Rbp o



(* compiling instructions  ------------------------------------------------- *)

(* | Addq | Subq | Imulq | Xorq | Orq | Andq
            | Shlq | Sarq | Shrq *)
let compile_bop : Ll.bop -> X86.opcode = function
  | Add -> Addq
  | Sub -> Subq
  | Mul -> Imulq
  | Shl -> Shlq
  | Lshr -> Shrq
  | Ashr -> Sarq
  | And -> Andq
  | Or -> Orq
  | Xor-> Xorq

let cmpl_binop (l:Alloc.loc) (b:bop) (t:ty) (op1:Alloc.operand) 
                                (op2:Alloc.operand) : X86.ins list =
  let x_op1 = compile_operand op1 in
  let x_op2 = compile_operand op2 in
  let dest = compile_operand (Alloc.Loc l) in
  let x_bop = compile_bop b in
  [ Movq, [x_op1; Reg R10]
  ; x_bop, [x_op2; Reg R10]
  ; Movq, [Reg R10; dest]
  ]


(* Helper function to compile LLVM CC to X86 CC *)
let cmpl_cnd : Ll.cnd -> X86.cnd = function
  | Eq -> Eq 
  | Ne -> Neq 
  | Sgt -> Gt 
  | Sge -> Ge 
  | Slt -> Lt 
  | Sle -> Le

(* - Alloca: needs to return a pointer into the stack *)
let cmpl_alloca (l:Alloc.loc) (t:ty) : X86.ins list =
  (* let dest = compile_operand (Alloc.Loc l) in *)
  (* [Subq, [Imm (Lit 8L); Reg Rsp]] *)
  []
  (* 
    Rsp -> R11

    Decrement Rsp by 8

  *)

(* - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier. *)

(* let cmpl_load (l:Alloc.loc) (t:ty) (op:Alloc.operand) : X86.ins list =
  let dest = compile_operand (Alloc.Loc l) in
  begin match op with
    | Alloc.Const _ | Alloc.Null-> failwith "invalid pointers"
    | Alloc.Gid gl -> 
      let x_op = compile_operand_base Rip op in
      [ Movq, [x_op; Reg R11]]
    | Alloc.Loc lo -> let x_op = compile_operand (Alloc.Loc lo) in
    [ Movq, [x_op; Reg R11]]
  end
  @
  [Movq, [Reg R11; dest]] *)
  
  (* Old store: *)
(* let cmpl_store (t:ty) (op1:Alloc.operand) (op2:Alloc.operand) : X86.ins list =
  let x_op1 = compile_operand op1 in
  let x_op2 = compile_operand op2 in
  [ Movq, [x_op1; Reg R11]
  ; Movq, [Reg R11; x_op2] 
  ] *)

(* New load: *)
let cmpl_load (l:Alloc.loc) (t:ty) (op:Alloc.operand) : X86.ins list =
  let dest = compile_operand (Alloc.Loc l) in
  begin match op with
    | Alloc.Const _ | Alloc.Null-> failwith "invalid pointers"
    | Alloc.Gid gl -> let x_op = compile_operand_base Rip op in
     Printf.printf "load from G: %s\n" (string_of_operand x_op);
      [ Movq, [x_op; Reg R11]] (* TODO: This is wrong, Leaq from global *)
    | Alloc.Loc lo ->
      let x_op = compile_operand (Alloc.Loc lo) in
      Printf.printf "load from: %s\n" (string_of_operand x_op);
      [ Movq, [x_op; Reg R10]
      ; Movq, [Ind2 R10; Reg R11]]
  end 
  @
  [Movq, [Reg R11; dest]
  ]

  (* ; Movq, [Ind2 R10; Reg R11]] *)

let cmpl_store (t:ty) (src:Alloc.operand) (dst_p:Alloc.operand) : X86.ins list =
  let x_src = compile_operand src in
  [ Movq, [x_src; Reg R11]] @ 

  begin match dst_p with
    | Alloc.Const _ | Alloc.Null-> failwith "invalid pointers"
    | Alloc.Gid gl -> let x_dst_p = compile_operand_base Rip dst_p in
      [ Movq, [Reg R11; x_dst_p]]
    | Alloc.Loc lo ->
      let x_dst_p = compile_operand (Alloc.Loc lo) in
      Printf.printf "store to: %s\n" (string_of_operand x_dst_p);
      [ Movq, [x_dst_p; Reg R10]
      ; Movq, [Reg R11; Ind2 R10]]
  end
 



(* - Br should jump *)
let cmpl_br (l:Alloc.loc) : X86.ins list =
  let dest = compile_operand (Alloc.Loc l) in
  [Jmp, [dest]]

(* - Cbr branch should treat its operand as a boolean conditional 
*)
let cmpl_cbr (op:Alloc.operand) (l1:Alloc.loc) (l2:Alloc.loc) : X86.ins list =
  let x_op = compile_operand op in
  let x_lbl1 = compile_operand (Alloc.Loc l1) in
  let x_lbl2 = compile_operand (Alloc.Loc l2) in
  (* Printf.printf "x_lbl1 = %s\n" (string_of_operand x_lbl1);
  Printf.printf "x_lbl2 = %s\n" (string_of_operand x_lbl2); *)
  [ Movq, [Imm (Lit 0L); Reg R11]
  ; Cmpq, [Reg R11; x_op] 
  ; J Eq,     [x_lbl2]
  ; Jmp,      [x_lbl1]
  ]

(*  - Icmp:  the Set instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.
  *)
let cmpl_icmp (l:Alloc.loc) (c:Ll.cnd) (t:ty) (op1:Alloc.operand) 
                                        (op2:Alloc.operand) : X86.ins list =
  let cc = cmpl_cnd c in
  (* Printf.printf "cnd = %s\n" (string_of_cnd cc); *)
  let dest = compile_operand (Alloc.Loc l) in
    (* begin match l with
    | Alloc.LStk ls -> 
    | _ -> failwith "can't handle this type here"
    end in *)
  (* Printf.printf "cmp = %s\n" (string_of_operand dest); *)
  let x_op1 = compile_operand op1 in
  let x_op2 = compile_operand op2 in
  (* Printf.printf "x_op1 = %s x_op2 = %s\n" (string_of_operand x_op1) (string_of_operand x_op2); *)
  [ Movq, [x_op1; Reg R10]
  ; Movq, [x_op2; Reg R11]
  ; Movq, [Imm (Lit 0L); dest]  (* zero-init dest *)
  ; Cmpq, [Reg R11; Reg R10] 
  ; Set cc, [dest]
  ] 


(* - Bitcast: does nothing interesting at the assembly level *)
let cmpl_bitcast (l:Alloc.loc) (t1:ty) (op:Alloc.operand) (t2:ty) : X86.ins list =
  let dest = compile_operand (Alloc.Loc l) in
  begin match op with
  | Alloc.Gid g -> 
    let x_op = compile_operand_base Rip op in [ Leaq, [x_op; Reg R11]]
  | _ -> let x_op = compile_operand op in [ Movq, [x_op; Reg R11]]
  end 
  @
  [Movq, [Reg R11; dest]]

(* 
- Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.
*)
let cmpl_ret (t:ty) (op:Alloc.operand option) : X86.ins list =
  let i = begin match op with
  | Some o -> (* Printf.printf "cmpl_ret: Some o \n"; *)
    let x_op = compile_operand o in
    [Movq, [x_op; Reg Rax]]
  | None -> []
  end in
  i @ 
  [ Movq, [Reg Rbp; Reg Rsp]
  ; Popq, [Reg Rbp]
  ; Retq, []
  ]


let cmpl_ilbl l : x86elt list = 
  begin match l with 
    | Alloc.LLbl lb -> [L (lb, false)]
    | _ -> failwith "don't know what to do with you"
  end



(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that 
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)


(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)

let arg_loc_base (n : int) (r: X86.reg) : operand = 
  begin match n with
  | 0 -> Reg Rdi
  | 1 -> Reg Rsi
  | 2 -> Reg Rdx
  | 3 -> Reg Rcx
  | 4 -> Reg R08
  | 5 -> Reg R09
  | _ -> Ind3 (Lit (Int64.of_int (8 * (n-4))), r)
  end

let arg_loc (n : int) : operand = arg_loc_base n Rbp
  

let compile_call_helper (i:X86.ins list * int) 
                        (os:ty * Alloc.operand) : X86.ins list * int =
  let ins, count = i in
  let typ, op = os in 

  let arg_ins = begin match op with
  | Alloc.Gid g -> let x_op = compile_operand_base Rip op in
        [Leaq, [x_op; Reg R10]]
  | _ -> let x_op = compile_operand op in [Movq, [x_op; Reg R10]]
  end in
  let dest = arg_loc count in
  let new_ins = 
    if count < 6 then
      [ Movq, [Reg R10; dest]
      ] 
    else
      [ Pushq, [Reg R10]]
    in
  (ins @ arg_ins @ new_ins, count + 1)



let compile_call (fo:Alloc.operand) (os:(ty * Alloc.operand) list) : x86stream = 
  let arg_ins, _ = List.fold_left compile_call_helper ([], 0) os in
  let fn = begin match fo with 
    | Alloc.Gid g -> Imm (Lbl g)
    | _ -> failwith "wrong type"
  end in 
  let num_args = Int64.of_int ((8 * List.length os)) in
  let call_ins = 
    [ Callq, [fn]] @
    if num_args > 0L then [Addq, [Imm (Lit num_args); Reg Rsp]] else []
    in lift (arg_ins @ call_ins)



(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelmentptr, you must generate x86 code that performs
   the appropriate arithemetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes. 
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition
   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty tdecls t : int =
  begin match t with 
  | I1 | I64 | Ptr _ -> 8
  | Array (n, t_elm) -> n * (size_ty tdecls t_elm)
  | Struct ts -> List.fold_left (fun sum c -> sum + size_ty tdecls c) 0 ts
  | Namedt s -> size_ty tdecls (List.assoc s tdecls)
  | _ -> 0 (* corresponds to Void, I8, Fun *)
  end

(* Generates code that computes a pointer value.  

   1. o must be a pointer of type t=*t'

   2. the value of o is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t' located at the base address

   4. subsequent indices are interpreted according to the type t':

     - if t' is a struct, the index must be a constant n and it 
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the 
       sizes of the types of the previous elements ]

     - if t' is an array, the index can be any operand, and its
       value determines the offset within the array.
 
     - if t' is any other type, the path is invalid

     - make sure you can handle named types!

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)

  let idx_helper (ii:int * int * int * tdc) (el:Ll.ty) : 
                                    (int * int * int * tdc) =
    let (n, i, acc, td) = ii in
    if i < n then
    let new_size = size_ty td el in
      n, i + 1, acc + new_size, td
    else 
      n, i + 1, acc, td

  let idx tdecls (c:int64) (t_lst:Ll.ty list) : int64 =
    let int_c = (Int64.to_int c) in
    let _, _, acc, _ = List.fold_left idx_helper (int_c, 0, 0, tdecls) t_lst in
    Int64.of_int acc

  let rec gep_helper tdecls (t:Ll.ty) (path:Alloc.operand list) : X86.ins list =
    begin match path with 
    | h::tl -> 
      let h_op = compile_operand_base Rbp h in
      begin match t with 
      | Struct st -> 
        begin match h with 
        | Alloc.Const c ->
          [ Addq, [Imm (Lit (idx tdecls c st)); Reg Rcx] 
          ] @ (gep_helper tdecls (List.nth st (Int64.to_int c)) tl)
        | _ -> failwith "cannot use this as an index"
        end
      | Array (a, tp) -> 
        let s = size_ty tdecls tp in 
        [ Movq, [h_op; Reg R10]
        ; Imulq, [Imm (Lit (Int64.of_int s)); Reg R10]
        ; Addq, [Reg R10; Reg Rcx]
        ] @ (gep_helper tdecls tp tl)
      | Namedt tp -> gep_helper tdecls (List.assoc tp tdecls) path
      | _ -> failwith "cannot calculate an offset with this type"
      end  
    | [] -> []
    end
  

let compile_getelementptr tdecls (t:Ll.ty) 
                        (o:Alloc.operand) (os:Alloc.operand list) : x86stream =
  begin match t with
  | Ptr p -> 
    let s = size_ty tdecls p in 
    let insns = 
    begin match os with 
    | h::tl -> 
      let h_op = compile_operand_base Rip h in (* the index of t' *)
      [ Movq, [ Imm (Lit 0L); Reg Rcx]     
      ; Movq, [ h_op; Reg R10]
      ; Imulq, [ Imm (Lit (Int64.of_int s)); Reg R10]
      ; Addq, [Reg R10; Reg Rcx]
      ] @ (gep_helper tdecls p tl) 
    | [] -> []
    end 
    
    @
    
    begin match o with
    | Alloc.Gid g -> 
      let base = compile_operand_base Rip o in
      [ Leaq, [base; Reg R11]]
    | _ -> let base = compile_operand_base Rbp o in
      [ Movq, [base; Reg R11]]
    end in
    lift (insns @ 
      (* 
        Rcx contains the offset
       *)
    [ Addq, [Reg Rcx; Reg R11] ]

          (* For testing START *)
    (* ; Movq, [Ind2 R10; Reg R11]] *)
          (* For testing END *)

  )
  | _ -> failwith "not a pointer"
  end 


let cmpl_gep tdecls (l:Alloc.loc) (t:ty) (op1:Alloc.operand)
                                     (opl:Alloc.operand list) : x86stream =
  let ins = compile_getelementptr tdecls t op1 opl in
  let dest = compile_operand (Alloc.Loc l) in
  lift ([Movq, [Reg R11; dest]]
  ) @ ins

(* compiling instructions within function bodies ---------------------------- *)

(* An Alloc.fbody value is a list of LLVM lite labels, instructions,
   and terminators.  The compile_fbody function can process each of these
   in sequence, generating a corresponding stream of x86 instructions.

   The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few tips:

   - The goal of this project is _not_ to produce efficient code. Emit
     extra moves liberally, using Rax and Rcx as scratch registers.
     You should aim for correctness first, making sure you don't
     violate restrictions of x86-64 assembly (e.g. the number of
     memory operands allowed for an instruction!)

   - The type of x86streams and their operations make appending to a
     stream efficient. You might find it useful to define a tail-
     recursive helper function that passes an output stream as an
     accumulator.
  
    type fbody = (loc * insn) list
    
    type loc =
    | LVoid                       (* no storage *)
    | LReg of X86.reg             (* x86 register *)
    | LStk of int                 (* a stack offset from %rbp *)
    | LLbl of X86.lbl             (* an assembler label *)
*)

let cmpl_call (l:Alloc.loc) (t:ty) (op:Alloc.operand) 
                                    (args:(ty * Alloc.operand) list) : x86stream =
  let insns = compile_call op args in
  let pre = begin match l with
  | Alloc.LVoid -> []
  | _ -> 
    let dest = compile_operand (Alloc.Loc l) in 
    lift [ Movq, [Reg Rax; dest]]
  end
   in
  pre @ insns
  
  


let compile_insn tdecls (l:Alloc.loc) (i:Alloc.insn) : x86stream =
  begin match i with
  | Alloc.ILbl -> cmpl_ilbl l
  | Alloc.Binop (b, t, opr1, opr2) -> lift @@ cmpl_binop l b t opr1 opr2
  | Alloc.Alloca t -> lift @@ cmpl_alloca l t
  | Alloc.Load  (t, opr) -> lift @@ cmpl_load l t opr 
  | Alloc.Store (t, opr1, opr2) -> lift @@ cmpl_store t opr1 opr2 
  | Alloc.Icmp (llcnd, t, opr1, opr2) -> lift @@ cmpl_icmp l llcnd t opr1 opr2
  | Alloc.Call (t, opr, args) -> cmpl_call l t opr args 
  | Alloc.Bitcast (t1, opr, t2) -> lift @@ cmpl_bitcast l t1 opr t2
  | Alloc.Gep (t, opr1, opr_list) -> cmpl_gep tdecls l t opr1 opr_list
  | Alloc.Ret (t, opr_option) -> lift @@ cmpl_ret t opr_option
  | Alloc.Br l -> lift @@ cmpl_br l
  | Alloc.Cbr (opr, l1, l2) -> lift @@ cmpl_cbr opr l1 l2
  end



let compile_body_helper (l: x86stream * (tid * ty) list)
                                    (el:Alloc.loc * Alloc.insn) : (x86stream * ((tid * ty) list)) =
  let _l, tdecls = l in
  let lo, li = el in
  (compile_insn tdecls lo li @ _l, tdecls)

let compile_fbody tdecls (af:Alloc.fbody) : x86stream =
  let insn, _ = List.fold_left compile_body_helper ([],tdecls) af in
  insn

(* compile_fdecl ------------------------------------------------------------ *)

(* We suggest that you create a helper function that computes the 
   layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id 
     is also stored as a stack slot.
   - uids associated with instructions that do not assign a value,
     such as Store and a Call of a Void function should be associated
     with Alloc.LVoid
   - LLVMlite uids and labels share a namespace. Block labels you encounter
     should be associated with Alloc.Llbl

*)


let layout_insn_classifier (m:layout * int) (l:uid * insn) : layout * int =
  let map, count = m in
  let new_count = count - 8 in
  let u, i = l in
  begin match i with
  | Store (x,_,_) -> 
    begin match x with
    | _ -> (map @ [(u, Alloc.LVoid)], count)
    (* | _ -> (map @ [(u, Alloc.LStk count)], new_count) *)
    end
  | Call (x,_,_) -> 
    begin match x with
    | Void -> (map @ [(u, Alloc.LVoid)], count)
    | _ -> (map @ [(u, Alloc.LStk count)], new_count)
    end
  
  (* | Alloca _ -> (map, count) *)
  | _ -> (map @ [(u, Alloc.LStk count)], new_count)
  end

let label_block_helper (m:layout * int) (b:lbl * block) : layout * int =
  let map, count = m in
  let label, blk = b in
  List.fold_left layout_insn_classifier 
                      (map @ [(label, Alloc.LLbl label)], count) blk.insns

let args_helper (u: uid) (m:layout * int * int) : layout * int * int =
  let map, count, arg_count = m in
  if count > 6 then 
    (map @ [(u, Alloc.LStk (8 * (count - 5)))], count - 1, arg_count)  
  else (* first 6 args *)
    let mul = (-8 * (arg_count - count + 1)) in
    (map @ [(u, Alloc.LStk mul)], count - 1, arg_count)
      
let stack_layout (f:Ll.fdecl) : layout =
  let entry_blk, lbld_blks = f.cfg in
  let args_count = List.length f.param in
  let map_w_args, _, _ = List.fold_right args_helper f.param ([], args_count, args_count) in
  let map_w_locals, c = List.fold_left layout_insn_classifier (map_w_args, -8 * (args_count+1)) entry_blk.insns in
  let final_map, _ = List.fold_left label_block_helper (map_w_locals, c) lbld_blks in
  final_map

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout). Use
     the provided alloc_cfg function to produce an allocated function
     body.

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)

let push_helper (l:X86.ins list * int) (u:uid)  : (X86.ins list * int) =
  let insns, count = l in
  let new_ins = (* if count < 6 then *)
    [(Pushq, [arg_loc_base count Rbp])]
  (* else []  *)in
  (new_ins @ insns, count + 1)

let gen_push_args_to_stack (arg_list:uid list) : X86.ins list =
  let push_insns, _ = List.fold_left push_helper ([],0) arg_list in
  push_insns


let count_helper (c:int) (el:uid * Alloc.loc) : int =
  let u, l = el in
  begin match l with
  | Alloc.LStk s -> (* Printf.printf "counting this %s %d \n" u (c+1); *) c + 1
  | _ -> c
  end

let count_local_variables (c:Ll.cfg) : int = 
  (* 
    type block = { 
      insns: (uid * insn) list; terminator: terminator }
   *)
  let entry_blk, lbld_blks = c in
  let map, _ = List.fold_left layout_insn_classifier ([], 0) entry_blk.insns in
  let final_map, _ = List.fold_left label_block_helper (map, 0) lbld_blks in
  (* let n =  *)
  List.fold_left count_helper 0 final_map 
  (*in  Printf.printf "Count == %d\n" n; n *)

let generate_prologue (f:Ll.fdecl) : X86.ins list = 
  let arg_list = f.param in
  let num_vars = count_local_variables f.cfg in
  [ Pushq,  [Reg Rbp]
  ; Movq,  [Reg Rsp; Reg Rbp]
  ] @ gen_push_args_to_stack arg_list
  @ if num_vars > 0 then
    [Subq, [Imm (Lit (Int64.of_int (8 * (num_vars)))); Reg Rsp]]
  else []

let generate_epilogue (l:layout) : X86.ins list = 
  [] (* TODO: subtract from Rsp *)

let compile_fdecl tdecls (g:gid) (f:Ll.fdecl) : x86stream =
  let l = stack_layout f in
  let prologue = generate_prologue f in
  let fbody = alloc_cfg l f.cfg in
  let body_insn = compile_fbody tdecls fbody in
  let epilogue = generate_epilogue l in
  (lift epilogue) @ body_insn @ (lift prologue) @ [L (Platform.mangle g, true)]

(* compile_gdecl ------------------------------------------------------------ *)

(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit = function
  | GNull      -> [Quad (Lit 0L)]
  | GGid gid   -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c     -> [Quad (Lit c)]
  | GString s  -> [Asciz s]
  | GArray gs 
  | GStruct gs -> List.map compile_gdecl gs |> List.flatten

and compile_gdecl (_, g) = compile_ginit g

(* compile_prog ------------------------------------------------------------- *)

let compile_prog {tdecls; gdecls; fdecls} : X86.prog =

  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) 
                                                    (compile_gdecl gdecl) in
  
  let f = fun (name, fdecl) -> prog_of_x86stream @@ compile_fdecl 
tdecls name fdecl in  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)