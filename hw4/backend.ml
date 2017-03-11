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
let map_operand f g : Ll.operand -> operand = function
  | Null -> Null
  | Const i -> Const i
  | Gid x -> Gid (g x)
  | Id u -> Loc (f u)

let map_insn f g : Ll.insn -> insn = 
  let mo = map_operand f g in function
  | Binop (b,t,o,o') -> Binop (b,t,mo o,mo o')
  | Alloca t         -> Alloca t
  | Load (t,o)       -> Load (t,mo o)
  | Store (t,o,o')   -> Store (t,mo o,mo o')
  | Icmp (c,t,o,o')  -> Icmp (c,t,mo o,mo o')
  | Call (t,o,args)  -> Call (t,mo o,List.map (fun (t,o) -> t, mo o) args)
  | Bitcast (t,o,t') -> Bitcast (t,mo o,t')
  | Gep (t,o,is)     -> Gep (t,mo o,List.map mo is)

let map_terminator f g : Ll.terminator -> insn = 
  let mo = map_operand f g in function
  | Ret (t,None)   -> Ret (t, None)
  | Ret (t,Some o) -> Ret (t, Some (mo o))
  | Br l           -> Br (f l)
  | Cbr (o,l,l')   -> Cbr (mo o,f l,f l')

let of_block f g (b:Ll.block) : fbody =
  List.map (fun (u,i) -> f u, map_insn f g i) b.insns
  @ [LVoid, map_terminator f g b.terminator]
                                
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


(* Once we have a layout, it's simple to generate the allocated version of our
   LLVMlite program *)
let alloc_cfg (layout:layout) (g:Ll.cfg) : Alloc.fbody =
  Alloc.of_cfg (fun x -> List.assoc x layout) 
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
let compile_operand : Alloc.operand -> X86.operand = 
  let open Alloc in function
  | Null -> Asm.(~$0)
  | Const i -> Asm.(Imm (Lit i))
  | Gid l -> Asm.(~$$l)
  | Loc LVoid -> failwith "compilng uid without location"
  | Loc (LStk i) -> Asm.(Ind3 (Lit (Int64.of_int @@ i * 8), Rbp))
  | Loc (LReg r) -> Asm.(~%r)
  | Loc (LLbl l) -> Asm.(Ind1 (Lbl l))


let movq_to_reg o r =
  match o with
  | Alloc.Gid g -> Leaq, [Ind3 (Lbl g, Rip); Reg r ]
  | _ -> Movq, [ compile_operand o; Reg r] 


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


let arg_reg : int -> (X86.reg) option = function
  | 0 -> Some Rdi
  | 1 -> Some Rsi
  | 2 -> Some Rdx
  | 3 -> Some Rcx
  | 4 -> Some R08
  | 5 -> Some R09
  | n -> None

let compile_call (fo:Alloc.operand) (os:(ty * Alloc.operand) list) : x86stream = 
  let opregs = List.mapi (fun i (_,o) -> o, arg_reg i) os in
  let nstack = List.length os - 6 in
  lift (List.map (fun (o, ro) ->
            match ro with
            | None   -> Asm.( Pushq, [compile_operand o] )
            | Some r -> Asm.( movq_to_reg o r )
          ) (List.rev opregs))
  >:: I Asm.( Callq, [compile_operand fo] )
  >@ lift (if nstack > 0 
           then Asm.[ Addq, [~$nstack; ~%Rsp] ]
           else Asm.[])

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
    | Void | I8 | Fun _ -> 0
    | I1 | I64 | Ptr _ -> 8 (* Target 64-bit only subset of X86 *)
    | Struct ts -> List.fold_left (fun acc t -> acc + (size_ty tdecls t)) 0 ts
    | Array (n, t) -> n * (size_ty tdecls t)
    | Namedt id -> size_ty tdecls (List.assoc id tdecls)
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

let compile_getelementptr tdecls (t:Ll.ty) 
                          (o:Alloc.operand) (os:Alloc.operand list) : x86stream =
  let at = match t with
    | Ptr at -> at | _ -> failwith "gep of non-pointer" in
  let rec expandt at =
    match at with
    | Namedt x -> expandt (List.assoc x tdecls)
    | _ -> at
  in
  let rec struct_offset ats i =
    if i <= 0L then 0 else 
      match ats with
      | [] -> failwith "malformed gep instructino"
      | t::ats' -> (size_ty tdecls t) + (struct_offset ats' (Int64.pred i))
  in
  lift Asm.[ movq_to_reg o Rax ]
  >@ fst (List.fold_left (fun (es,at) o ->
              match expandt at, o with
              | Ll.Struct ats, Alloc.Const i -> 
                 es >:: I Asm.( Addq, [~$(struct_offset ats i); ~%Rax] ),
                 List.nth ats (Int64.to_int i)
              | Ll.Array (_,Ll.I8), _ 
                | Ll.Array (_,Ll.I1), _ -> 
                 es >:: I Asm.( Addq,  [compile_operand o; ~%Rax] ),
                 Ll.(I8)
              | Ll.Array (_,t), _ ->
                 es >@ lift Asm.[ movq_to_reg o Rcx
                                ; Imulq, [~$(size_ty tdecls t); ~%Rcx]
                                ; Addq,  [~%Rcx; ~%Rax] ],
                 t
              | _ -> failwith "malformed gep instruction"
            ) ([],Array (0,at)) os)

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

   - Icmp:  the Set instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level

   Compiling block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional
*)

let compile_fbody tdecls (af:Alloc.fbody) : x86stream =
  let rec loop (af:Alloc.fbody) (outstream:x86stream) : x86stream =
    let cb = function
      | Ll.Add ->  Addq | Ll.Sub ->  Subq | Ll.Mul ->  Imulq
      | Ll.Shl ->  Shlq | Ll.Lshr -> Shrq | Ll.Ashr -> Sarq 
      | Ll.And ->  Andq | Ll.Or ->   Orq  | Ll.Xor ->  Xorq in
    let cc = function
      | Ll.Eq  -> Set Eq | Ll.Ne  -> Set Neq | Ll.Slt -> Set Lt
      | Ll.Sle -> Set Le | Ll.Sgt -> Set Gt  | Ll.Sge -> Set Ge in
    let co = compile_operand in

    let open Alloc in
    match af with
    | [] -> outstream

    | (LLbl l, ILbl)::rest ->
       loop rest @@ 
         (outstream
          >@ [L (l, false)])

    | (x, Icmp (c,_,o,o'))::rest -> 
       let dst = compile_operand (Loc x) in
       loop rest @@
         ( outstream
           >@ lift Asm.[ movq_to_reg o Rax
                       ; movq_to_reg o' Rcx
                       ; Cmpq,       [~%Rcx; ~%Rax]
                       ; cc c,       [dst] 
                       ; Andq,       [~$1; dst] ] )

    | (x, Binop (b,_,o,o'))::rest ->
       loop rest @@
         ( outstream
           >@ lift Asm.[ movq_to_reg o Rax
                       ; movq_to_reg o' Rcx
                       ; cb b,       [~%Rcx; ~%Rax]
                       ; Movq,       [~%Rax; co (Loc x)] ] )

    | (x, Alloca at)::rest ->
       loop rest @@
         ( outstream
           >@ lift Asm.[ Subq, [~$(size_ty tdecls at); ~%Rsp]
                       ; Movq, [~%Rsp; co (Loc x)] ] )

    | (x, Bitcast (_,o,_))::rest ->
       loop rest @@ 
         ( outstream
           >@ lift Asm.[ movq_to_reg o Rax
                       ; Movq, [~%Rax; co (Loc x)] ] )

    | (x, Load (_, src))::rest ->
       loop rest @@
         ( outstream 
           >@ lift Asm.[ movq_to_reg src Rax
                       ; Movq, [Ind2 Rax; ~%Rcx]
                       ; Movq, [~%Rcx; co (Loc x)] ] )

    | (LVoid, Store (_,src,dst))::rest ->
       loop rest @@ 
         ( outstream 
           >@ lift Asm.[ movq_to_reg src Rax
                       ; movq_to_reg dst Rcx
                       ; Movq, [~%Rax; Ind2 Rcx] ] )
      
    | (x, Gep (at,o,os))::rest -> 
       loop rest @@ 
         ( outstream
           >@ compile_getelementptr tdecls at o os
           >:: I Asm.( Movq, [~%Rax; co (Loc x)] ) )

    | (x, Call (t,fo,os))::rest ->
       loop rest @@ 
         ( outstream 
           >@ compile_call fo os
           >@ (if t = Ll.Void 
               then [] 
               else lift Asm.[ Movq, [~%Rax; co (Loc x)] ]) )

    | (LVoid, Ret (_,None))::rest ->
       loop rest @@ 
         ( outstream
           >@ lift Asm.[ Movq, [~%Rbp; ~%Rsp]
                       ; Popq, [~%Rbp]
                       ; Retq, [] ] )

    | (LVoid, Ret (_,Some o))::rest ->
       loop rest @@ 
         ( outstream
           >@ lift Asm.[ movq_to_reg o Rax 
                       ; Movq, [~%Rbp; ~%Rsp]
                       ; Popq, [~%Rbp]
                       ; Retq, [] ] )

    | (LVoid, Br (LLbl l))::rest ->
       loop rest @@ 
         ( outstream
           >:: I Asm.( Jmp, [~$$l] ) )

    | (LVoid, Cbr (o,(LLbl l1),(LLbl l2)))::rest ->
       loop rest @@ 
         ( outstream
           >@ lift Asm.[ movq_to_reg o Rax
                       ; Cmpq,  [~$0; ~%Rax]
                       ; J Neq, [~$$l1]
                       ; Jmp,   [~$$l2] ] )

    | _ -> failwith "codegen failed to find instruction"
  in
  loop af []

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
let stack_layout (f:Ll.fdecl) : layout =
  let layout_iis = 
    List.fold_left (fun (lo, n) (u, i) -> 
        match i with
        | Ll.Call (Ll.Void, _, _)
        | Ll.Store _ -> (u,Alloc.LVoid)::lo, n
        | _ -> (u,Alloc.LStk n)::lo, pred n
      ) in
  let layout_lbl_block (lo, n) (l, b) =
    layout_iis ((l,Alloc.LLbl (Platform.mangle l))::lo, n) 
               b.insns in
  let lo, n = List.fold_left
                (fun (lo, n) x -> (x, Alloc.LStk n)::lo, pred n
                ) ([], -1) f.param in
  let e,bs = f.cfg in
  let lo, n  = layout_iis (lo, n) e.insns in
  let lo', _ = List.fold_left layout_lbl_block (lo, n) bs in
  lo'


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

(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : operand =
  let rbp_offset n = Ind3 (Lit (Int64.of_int @@ 8 * n), Rbp) in
  begin match arg_reg n with
    | Some r -> Reg r
    | None -> rbp_offset (n-4)
  end

let compile_fdecl tdecls (g:gid) (f:Ll.fdecl) : x86stream =
  let layout = stack_layout f in
  let afbody = alloc_cfg layout f.cfg in
  let is_stk = function
    | _, Alloc.LStk _ -> true | _ -> false in
  let tmpsize = 8 * List.(length (filter is_stk layout)) in
  [L (Platform.mangle g, true)]
  >@ lift Asm.[ Pushq, [~%Rbp]
              ; Movq,  [~%Rsp; ~%Rbp]
              ; Subq,  [~$tmpsize; ~%Rsp] ]
  >@ List.(flatten @@ mapi (fun i x ->
     lift Asm.[ Movq, [arg_loc i; ~%Rax]
              ; Movq, [~%Rax; compile_operand (Alloc.Loc (List.assoc x layout))] ]
           ) f.param)
  >@ compile_fbody tdecls afbody


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
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> prog_of_x86stream @@ compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
