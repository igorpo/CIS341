(* X86lite language representation. *)

(* assembler syntax --------------------------------------------------------- *)

(* Labels for code blocks and global data. *)
type lbl = string

type quad = int64

(* Immediate operands *)
type imm = Lit of quad
         | Lbl of lbl

(* Registers:
    instruction pointer: rip
    arguments: rdi, rsi, rdx, rcx, r09, r08
    callee-save: rbx, rbp, r12-r15 
*)
type reg = Rip
         | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp
         | R08 | R09 | R10 | R11 | R12 | R13 | R14 | R15

type operand = Imm of imm            (* immediate *)
             | Reg of reg            (* register *)
             | Ind1 of imm           (* indirect: displacement *)
             | Ind2 of reg           (* indirect: (%reg) *)
             | Ind3 of (imm * reg)   (* indirect: displacement(%reg) *)

(* Condition Codes *)
type cnd = Eq | Neq | Gt | Ge | Lt | Le

type opcode = Movq | Pushq | Popq
            | Leaq
            | Incq | Decq | Negq | Notq
            | Addq | Subq | Imulq | Xorq | Orq | Andq
            | Shlq | Sarq | Shrq
            | Jmp | J of cnd
            | Cmpq  | Set of cnd
            | Callq | Retq

(* An instruction is an opcode plus its operands.
   Note that arity and other constraints about the operands 
   are not checked. *)
type ins = opcode * operand list              

type data = Asciz of string
          | Quad of imm

type asm = Text of ins list    (* code *)
         | Data of data list   (* data *)

(* labeled blocks of data or code *)
type elem = { lbl: lbl; global: bool; asm: asm }

type prog = elem list

(* Provide some syntactic sugar for writing x86 code in OCaml files. *)
module Asm = struct
  let (~$) i = Imm (Lit (Int64.of_int i))      (* int64 constants *)
  let (~$$) l = Imm (Lbl l)                    (* label constants *)
  let (~%) r = Reg r                           (* registers *)

  (* helper functions for building blocks of data or code *)
  let data l ds = { lbl = l; global = true; asm = Data ds }
  let text l is = { lbl = l; global = false; asm = Text is }
  let gtext l is = { lbl = l; global = true; asm = Text is }
end

(* pretty printing ----------------------------------------------------------- *)

let string_of_reg : reg -> string = function
  | Rip -> "%rip"
  | Rax -> "%rax" | Rbx -> "%rbx" | Rcx -> "%rcx" | Rdx -> "%rdx"
  | Rsi -> "%rsi" | Rdi -> "%rdi" | Rbp -> "%rbp" | Rsp -> "%rsp"
  | R08 -> "%r8"  | R09 -> "%r9"  | R10 -> "%r10" | R11 -> "%r11"
  | R12 -> "%r12" | R13 -> "%r13" | R14 -> "%r14" | R15 -> "%r15"

let string_of_lbl (l:lbl) : string = l

let string_of_imm : imm -> string = function
  | Lit i -> Int64.to_string i
  | Lbl l -> string_of_lbl l

let string_of_operand : operand -> string = function
  | Imm i -> "$" ^ string_of_imm i
  | Reg r -> string_of_reg r
  | Ind1 i -> string_of_imm i
  | Ind2 r -> "(" ^ string_of_reg r ^ ")"
  | Ind3 (i, r) -> string_of_imm i ^ "(" ^ string_of_reg r ^ ")"

let string_of_jmp_operand : operand -> string = function
  | Imm i -> string_of_imm i
  | Reg r -> "*" ^ string_of_reg r
  | Ind1 i -> "*" ^ string_of_imm i
  | Ind2 r -> "*" ^ "(" ^ string_of_reg r ^ ")"
  | Ind3 (i, r) -> "*" ^ string_of_imm i ^ "(" ^ string_of_reg r ^ ")"

let string_of_cnd : cnd -> string = function
  | Eq -> "e"  | Neq -> "ne" | Gt -> "g"
  | Ge -> "ge" | Lt -> "l"   | Le -> "le"

let string_of_opcode : opcode -> string = function
  | Movq -> "movq" | Pushq -> "pushq" | Popq -> "popq"
  | Leaq -> "leaq"
  | Incq -> "incq" | Decq -> "decq" | Negq -> "negq" | Notq -> "notq"
  | Addq -> "addq" | Subq -> "subq" | Imulq -> "imulq"
  | Xorq -> "xorq" | Orq -> "orq"  | Andq -> "andq"
  | Shlq -> "shlq" | Sarq -> "sarq" | Shrq -> "shrq"
  | Jmp  -> "jmp"  | J c -> "j" ^ string_of_cnd c 
  | Cmpq -> "cmpq" | Set c -> "set" ^ string_of_cnd c
  | Callq -> "callq" | Retq -> "retq"

let map_concat s f l = String.concat s @@ List.map f l

let string_of_shift op = function
  | [ Imm i ; dst ] as args ->
    "\t" ^ string_of_opcode op ^ "\t" ^ map_concat ", " string_of_operand args
  | [ Reg Rcx ; dst ] ->
    Printf.sprintf "\t%s\t%%cl, %s" (string_of_opcode op) (string_of_operand dst)
  | args -> failwith (Printf.sprintf "shift instruction has invalid operands: %s\n" 
                     (map_concat ", " string_of_operand args))
                   
let string_of_ins (op, args: ins) : string =
  match op with
  | Shlq | Sarq | Shrq -> string_of_shift op args
  | _ ->
  let f = match op with
    | J _ | Jmp | Callq -> string_of_jmp_operand 
    | _ -> string_of_operand 
  in
  "\t" ^ string_of_opcode op ^ "\t" ^ map_concat ", " f args

let string_of_data : data -> string = function
  | Asciz s -> "\t.asciz\t" ^ "\"" ^ s ^ "\""
  | Quad i -> "\t.quad\t" ^ string_of_imm i

let string_of_asm : asm -> string = function
  | Text is -> "\t.text\n" ^ map_concat "\n" string_of_ins is
  | Data ds -> "\t.data\n" ^ map_concat "\n" string_of_data ds

let string_of_elem {lbl; global; asm} : string =
  let sec, body = match asm with
    | Text is -> "\t.text\n", map_concat "\n" string_of_ins is
    | Data ds -> "\t.data\n", map_concat "\n" string_of_data ds
  in
  let glb = if global then "\t.globl\t" ^ string_of_lbl lbl ^ "\n" else "" in
  sec ^ glb ^ string_of_lbl lbl ^ ":\n" ^ body

let string_of_prog (p:prog) : string =
  String.concat "\n" @@ List.map string_of_elem p
