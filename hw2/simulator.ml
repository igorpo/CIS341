(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 4L                (* assume we have a 4-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly four consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next three bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsB0 (Decq,  [~%Rdi])
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd, 3rd, or 4th byte of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. *)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun x -> 
  begin match x with 
  | Eq -> fz 
  | Neq -> not fz
  | Gt -> (fs = fo) && not fz (* not LE *)
  | Ge -> fs = fo
  | Lt -> fs <> fo
  | Le -> (fs <> fo) || fz
  end


(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  let addr_check = addr >= mem_bot && addr < mem_top in
  if addr_check then
    Some (Int64.to_int (Int64.sub addr mem_bot))
  else
    None 


(* Interprets operands *)
let interp_operand (op:operand) (m:mach) : int64 =
  begin match op with
  | Imm i -> 
    begin match i with
    | Lit l -> l  
    | Lbl _ -> invalid_arg "Attempted to interpret a label"
    end
  | Reg r -> m.regs.(rind r)
  | Ind1 i -> 
    begin match i with
    | Lit l -> 0L
    | Lbl _ -> invalid_arg "Attempted to interpret a label"
    end
  | Ind2 i -> 0L
  | Ind3 (i, r) -> 0L
  end

(* Interprets instruction *)
(*     
    - perform instruction
    - update the registers and/or memory appropriately
    - set the condition flags 
*)
let exec_ins (inst:ins) (m:mach) : unit =
  let opc, opl = inst in 
  begin match opc with
  | Movq -> ()
  | Pushq -> ()
  | Popq -> ()
  | Leaq -> ()
  | Incq -> ()
  | Decq -> ()
  | Negq -> ()
  | Notq -> ()
  | Addq -> ()
  | Subq -> ()
  | Imulq -> ()
  | Xorq -> ()
  | Orq -> ()
  | Andq -> () 
  | Shlq -> ()
  | Sarq -> ()
  | Shrq -> ()    
  | Jmp -> ()
  | J j -> ()
  | Cmpq -> ()
  | Set s -> ()
  | Callq -> ()
  | Retq -> ()
  end


(* Update flags *)
let update_flags (f:flags) (fo:bool) (fs:bool) (fz:bool) : unit =
  f.fo <- fo; f.fs <- fs; f.fz <- fz    

(* Initializes machine state *)
let init_state (m:mach) : unit =
  update_flags m.flags false false false

(* Resolves label *)
(* let resolve_lbl (l:lbl) () *)

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)

let step (m:mach) : unit =
  let rip = m.regs.(rind Rip) in
  let inst = m.mem.(Int64.to_int rip) in
  begin match inst with
  | InsB0 i -> exec_ins i m
  | _ -> () (* TODO: do we need to anything with byte?? *)
  end



(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl



 (* 
  We know that:
    1. size of data = len(s) + 1
    2. size of Lit = 8
    3. size of ins = 4 
  *)
let data_size_helper (s:int64) (d:data) : int64 = 
  begin match d with
  | Asciz a -> 
    let len = Int64.of_int (String.length a) in
    Int64.add s (Int64.add 1L len)
  | Quad q -> 
    begin match q with
    | Lit l -> Int64.add s 8L
    | Lbl l -> 
      let len = Int64.of_int (String.length l) in 
      Int64.add len (Int64.add s 1L) 
    end
  end


let compute_size (s:(int64 * int64)) (e:elem) : (int64 * int64) =
  let size_t, size_d = s in
  begin match e.asm with
  | Text t -> 
    let list_size = (Int64.of_int (List.length t)) in
    (Int64.add size_t (Int64.mul list_size 4L), size_d)
  | Data d -> (size_t, Int64.add size_d (List.fold_left data_size_helper 0L d))
  end

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
  let size_text, size_data = List.fold_left compute_size (0L, 0L) p in
  {entry=0L; text_pos=mem_bot; data_pos=Int64.add mem_bot size_text;
   text_seg=[]; data_seg=[]}


(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
