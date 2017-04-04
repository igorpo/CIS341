open X86
open Cunit

let hello_label = mk_lbl_named "hellostr"
let puts_label = mk_lbl_named "_puts"  (* gcc on linux/mac uses _ to munge names *)

let main_seq = [
  Push (esp);
  Mov (ebp, esp);
  
  Add (esp, Imm (-8l));   (* Not sure why this has to be 8 *)
  Mov (stack_offset 0l, Lbl hello_label);
  Call (Lbl puts_label);
  
  Mov (esp, ebp);
  Pop (ebp);
  Ret
]

let main_bb = {
  (mk_insn_block (mk_lbl_named "_main") main_seq) with
  global = true
}

let hello_data = {
  link = false;
  label = (mk_lbl_named "hellostr");
  value = GStringz "Hello, world!"
}

let cu = [Data hello_data; Code main_bb]

let _ =
  print_endline (string_of_cunit cu)
