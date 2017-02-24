open Ll
open Arg
open Assert

(* configuration flags ------------------------------------------------------ *)
let print_ll = ref false
let print_x86 = ref false
let clang = ref false
let assemble = ref true
let link = ref true
let executable_filename = ref "a.out"
let execute_x86 = ref false

let link_files = ref []
let add_link_file path =
  link_files := path :: (!link_files)

exception Ran_tests
let suite = ref (Providedtests.provided_tests @ Gradedtests.graded_tests)

let execute_tests () =
  Platform.configure ();
  let outcome = run_suite !suite in
  Printf.printf "%s\n" (outcome_to_string outcome);
  raise Ran_tests

let process_ll_file path file =
  let _ = Platform.verb @@ Printf.sprintf "* processing file: %s\n" path in
  let ll_ast = Driver.parse_file path in
  let _ = if !print_ll then Driver.print_ll file ll_ast in
  let dot_s_file = Platform.gen_name !Platform.output_path file ".s" in
  let dot_o_file = Platform.gen_name !Platform.output_path file ".o" in
  let _ =
    if !clang then begin
      Platform.verb "* compiling with clang";
      Platform.clang_compile path dot_s_file;
      Driver.print_banner dot_s_file;
      if !print_x86 then Platform.sh (Printf.sprintf "cat %s" dot_s_file) Platform.raise_error
    end else begin
      let asm_ast = Backend.compile_prog ll_ast in
      let asm_str = X86.string_of_prog asm_ast in
      let _ = if !print_x86 then Driver.print_x86 dot_s_file asm_str in
      let _ = Driver.write_file dot_s_file asm_str in
      ()
    end
  in
  let _ = if !assemble then Platform.assemble dot_s_file dot_o_file in
  let _ = add_link_file dot_o_file in 
  ()

let process_file path =
  let basename, ext = Platform.path_to_basename_ext path in
  begin match ext with
    | "ll" -> process_ll_file path basename
    | "o" -> add_link_file path
    | "c" -> add_link_file path
    | _ -> failwith @@ Printf.sprintf "found unsupported file type: %s" path
  end
 
let process_files files =
  if (List.length files) > 0 then begin
    List.iter process_file files;
  ( if !link then
      Platform.link (List.rev !link_files) !executable_filename );
  ( if !execute_x86 then
      let ret = Driver.run_executable "" !executable_filename in
      Driver.print_banner @@ Printf.sprintf "Executing: %s" !executable_filename;
      Printf.printf "* %s returned %d\n" !executable_filename ret )
  end
    
let args =
  [ ("-linux", Set Platform.linux, "use linux-style name mangling [must preceed --test on linux]")
  ; ("--test", Unit execute_tests, "run the test suite, ignoring other files inputs")
  ; ("-op", Set_string Platform.output_path, "set the path to the output files directory  [default='output']")
  ; ("-o", Set_string executable_filename, "set the name of the resulting executable [default='a.out']")    
  ; ("-S", Clear assemble, "stop after generating .s files; do generate .o files")
  ; ("-c", Clear link, "stop after generating .o files; do not generate executables")
  ; ("--print-ll", Set print_ll, "prints the program's LL code (after lowering to clang code if --clang-malloc is set)")
  ; ("--clang", Set clang, "compiles to assembly using clang, not the 341 backend (implies --clang-malloc)")
  ; ("--print-x86", Set print_x86, "prints the program's assembly code")
  ; ("--execute-x86", Set execute_x86, "run the resulting executable file")
  ; ("-v", Set Platform.verbose, "enables more verbose compilation output")
  ] 

let files = ref []

let _ =
  try
    Arg.parse args (fun filename -> files := filename :: !files)
      "CIS 341 main test harness\n\
       USAGE: ./main.native [options] <files>\n\
       see README for details about using the compiler";
    Platform.configure ();
    process_files !files

  with Ran_tests ->
    ()
