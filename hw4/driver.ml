open Printf
open Platform

(* configuration flags ------------------------------------------------------ *)
let print_ll_flag = ref false
let print_x86_flag = ref false
let print_ast_flag = ref false
let print_oat_flag = ref false
let clang = ref false
let assemble = ref true
let link = ref true
let executable_filename = ref "a.out"
let execute_x86 = ref false

let link_files = ref []
let add_link_file path =
  link_files := path :: (!link_files)
let files : string list ref = ref []

let print_banner s =
  let rec dashes n = if n = 0 then "" else "-"^(dashes (n-1)) in
  printf "%s %s\n%!" (dashes (79 - (String.length s))) s

let print_ll file ll_ast =
    print_banner (file ^ ".ll");
    print_endline (Ll.string_of_prog ll_ast)

let print_x86 file asm_str =
    print_banner file;
    print_endline asm_str

let read_file (file:string) : string =
  let channel = open_in file in
  let lines = ref [] in
  try while true; do
      lines := input_line channel :: !lines
  done; ""
  with End_of_file ->
    close_in channel;
    String.concat "\n" (List.rev !lines)

let write_file (file:string) (out:string) =
  let channel = open_out file in
  fprintf channel "%s" out;
  close_out channel

let parse_ll_file filename =
  let program = read_file filename |> 
                Lexing.from_string |>
                Llparser.prog Lllexer.token
  in
  program

let parse_oat_file filename =
  let lexbuf = read_file filename |> 
               Lexing.from_string
  in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Parser.Error -> failwith @@ Printf.sprintf "Parse error at: %s"
      (Range.string_of_range (Range.lex_range lexbuf))
    

let print_oat file ll_ast =
    print_banner (file ^ ".oat");
    Astlib.print_prog ll_ast

let print_ast p = Printf.printf "\n%s\n\n" (Astlib.ml_string_of_prog p)

let run_executable arg pr =
  let cmd = sprintf "%s%s %s" dot_path pr arg in
  sh cmd (fun _ i -> i)
  
let run_executable_to_tmpfile arg pr tmp =
  let cmd = sprintf "%s%s %d > %s 2>&1" dot_path pr arg tmp in
  sh cmd ignore_error

let run_program (args:string) (executable:string) (tmp_out:string) : string =
  let cmd = sprintf "%s%s %s > %s 2>&1" dot_path executable args tmp_out in
  let result = sh cmd (fun _ i -> i) in
  (read_file tmp_out) ^ (string_of_int result)

let process_ll_ast path file ll_ast =
  if !print_ll_flag then print_ll file ll_ast;
  let dot_s_file = Platform.gen_name !Platform.output_path file ".s" in
  let dot_o_file = Platform.gen_name !Platform.output_path file ".o" in
  if !clang 
  then (Platform.verb "* compiling with clang\n";
        Platform.clang_compile path dot_s_file;
        if !print_x86_flag 
        then (print_banner dot_s_file;
              Platform.sh (Printf.sprintf "cat %s" dot_s_file) Platform.raise_error))
  else (let asm_ast = Backend.compile_prog ll_ast in
        let asm_str = X86.string_of_prog asm_ast in
        if !print_x86_flag then print_x86 dot_s_file asm_str;
        write_file dot_s_file asm_str);
  if !assemble then Platform.assemble dot_s_file dot_o_file !opt_level;
  add_link_file dot_o_file

let string_of_ll_ast path ll_ast =
  let ll_str = Ll.string_of_prog ll_ast in
  let prog = Printf.sprintf "; generated from: %s\ntarget triple = \"%s\"\n%s\n"
      path !Platform.target_triple ll_str in
  prog

let process_ll_file path file =
  Platform.verb @@ Printf.sprintf "* processing file: %s\n" path;
  let ll_ast = parse_ll_file path in
  process_ll_ast path file ll_ast

let process_oat_ast path file oat_ast =
  if !print_oat_flag then print_oat file oat_ast;
  if !print_ast_flag then print_ast oat_ast;
  let ll_ast = Frontend.cmp_prog oat_ast in
  let dot_ll_file = Platform.gen_name !Platform.output_path file ".ll" in
  Platform.verb @@ Printf.sprintf "writing file: %s\n" dot_ll_file;
  let prog = string_of_ll_ast path ll_ast in
  write_file dot_ll_file prog;
  process_ll_ast dot_ll_file file ll_ast 

let process_oat_file path basename =
  Platform.verb @@ Printf.sprintf "* processing file: %s\n" path;
  let oat_ast = parse_oat_file path in
  process_oat_ast path basename oat_ast

let process_file path =
  let basename, ext = Platform.path_to_basename_ext path in
  begin match ext with
    | "oat" -> process_oat_file path basename
    | "ll" -> process_ll_file path basename
    | "o" -> add_link_file path
    | "c" -> add_link_file path
    | _ -> failwith @@ Printf.sprintf "found unsupported file type: %s" path
  end

let process_files files =
  List.iter process_file files;
  if !link 
  then (Platform.link (List.rev !link_files) !executable_filename;
        if !execute_x86 
        then (print_banner @@ Printf.sprintf "Executing: %s" !executable_filename;
              let ret = run_executable "" !executable_filename in
              Printf.printf "\n* %s returned %d\n" !executable_filename ret))
