open Assert
open X86
open Driver
open Ll
open Backend
open Analysistests
open Datastructures

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your project.                   *)

(* These tests will be used to grade your assignment *)

let fdecl_of_path path =
  Platform.verb @@ Printf.sprintf "* processing file: %s\n" path;
  let ll_ast = parse_ll_file path in
  match ll_ast.Ll.fdecls with
  | [_, fdecl] -> fdecl
  | _ -> failwith "test expected one fdecl"

let ll_dfa_file_test path compare analyze expected =
  let fdecl = fdecl_of_path path in
  let dfa = analyze (Cfg.of_ast fdecl) in
  compare dfa expected

let throw_key_diff compare a b =
  let keys = LblM.diff_keys compare a b in
  if List.length keys == 0 then ()
  else failwith @@ "Output differs at labels: " ^ String.concat ", " @@ keys

let executed_alias_file tests =
  let open Alias in
  let analyze f = Graph.dfa (analyze f) in
  List.map (fun (path, ans) ->
    ("alias: " ^ path, 
     fun () -> ll_dfa_file_test path (throw_key_diff Fact.compare) analyze ans)) tests

let executed_liveness_file (tests : (string * 'a Datastructures.LblM.t) list) =
  let open Liveness in
  let analyze f = Graph.dfa (analyze f) in
  List.map (fun (path, ans) -> 
    ("liveness: " ^ path, 
     fun () -> ll_dfa_file_test path (throw_key_diff Fact.compare) analyze ans)) tests

let exec_e2e_ast ll_ast args extra_files =
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let _ = Backend.set_liveness "dataflow" in
  let _ = Backend.set_regalloc "live" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::extra_files) exec_file in
  let result = Driver.run_executable args exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable exited with: %d\n" result in
  Int64.of_int result
  

let exec_e2e_file path args =
  let ast = Driver.parse_ll_file path in
  exec_e2e_ast ast args []

let io_test path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in  
  let _ = Backend.set_liveness "dataflow" in
  let _ = Backend.set_regalloc "live" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::["cinterop.c"]) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_program args exec_file tmp_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_s_file exec_file tmp_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let c_link_test c_files path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::c_files) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_executable args exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
    Int64.of_int result

let oat_file_test path args =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in
  
  let output_path = !Platform.output_path in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in  

  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast;
  let ll_ast = Frontend.cmp_prog oat_ast in
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let () = write_file dot_ll_file ll_str in
  let () = Platform.link (dot_ll_file::["runtime.c"]) exec_file in

  let result = Driver.run_program args exec_file tmp_file in
  let () = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file) Platform.ignore_error in
  let () = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result


let executed tests =
  List.map (fun (fn, ans) ->
      fn, assert_eqf (fun () -> exec_e2e_file fn "") ans)
    tests

let executed_oat_file tests =
  List.map (fun (path, args, ans) ->
      (path ^ " args: " ^ args), assert_eqf (fun () -> oat_file_test path args) ans)
    tests

let assert_quality fn ll_ast =
  let open Registers in
  let open Backend in
  let _ = set_liveness "dataflow" in
  let _ = set_regalloc "live" in
  let asm_ast = compile_prog ll_ast in
  let (h_opt,size_opt) = histogram_of_prog asm_ast in
  let score_opt = summary h_opt in 

  let _ = set_liveness "trivial" in
  let _ = set_regalloc "simple" in
  let asm_ast = compile_prog ll_ast in
  let (h_default,size_default) = histogram_of_prog asm_ast in
  let score_default = summary h_default in
  
  (* let score = (score_opt - score_default) + 2 * (size_default - size_opt) in *)
  let score = (score_opt - score_default) + 3 * (size_default - size_opt) in
  (* (summary(yours) - summary(simple)) + 2 * (size(simple) - size(yours)) *)
  
  (* Printf.printf "\n\n\n(summary(yours) = %d\n" score_opt;
  Printf.printf "summary(simple) = %d\n" score_default;
  Printf.printf "size(simple) = %d\n" size_default;
  Printf.printf "size(yours) = %d\n\n\n" size_opt; *)
  
  let _ = Platform.verb @@
    Printf.sprintf "opt: %5d %5d  none: %5d %5d = score %5d for %s\n" score_opt size_opt score_default size_default score fn
  in
  if score > 0 then () else failwith @@ Printf.sprintf "default better than opt score = %d" score
  

let assert_quality_ll fn () = 
  let ll_ast = Driver.parse_ll_file fn in
  assert_quality fn ll_ast

let quality_ll tests =
  List.map (fun (fn, _) -> fn, assert_quality_ll fn) tests

let assert_quality_oat fn () =
  let oat_ast = parse_oat_file fn in
  let ll_ast = Frontend.cmp_prog oat_ast in
  assert_quality fn ll_ast

let quality_oat tests =
  List.map (fun (fn, _, _) -> fn, assert_quality_oat fn) tests

let executed_io tests =
  List.map (fun (fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqf (fun () -> io_test fn args) ans)
    tests

let executed_c_link tests =
  List.map (fun (c_file, fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqf (fun () -> c_link_test c_file fn args) ans)
    tests

let binop_tests =
  [ "llprograms/add.ll", 14L
  ; "llprograms/sub.ll", 1L
  ; "llprograms/mul.ll", 45L
  ; "llprograms/and.ll", 0L
  ; "llprograms/or.ll",  1L
  ; "llprograms/xor.ll", 0L
  ; "llprograms/shl.ll", 168L
  ; "llprograms/lshr.ll", 10L
  ; "llprograms/ashr.ll", 5L ]

let calling_convention_tests =
  [ "llprograms/call.ll", 42L
  ; "llprograms/call1.ll", 17L 
  ; "llprograms/call2.ll", 19L
  ; "llprograms/call3.ll", 34L
  ; "llprograms/call4.ll", 34L
  ; "llprograms/call5.ll", 24L
  ; "llprograms/call6.ll", 26L            
  ; "llprograms/call7.ll", 7L
  ; "llprograms/call8.ll", 21L
  ]

let memory_tests =
  [ "llprograms/alloca1.ll", 17L
  ; "llprograms/alloca2.ll", 17L
  ; "llprograms/global1.ll", 12L    
  ]

let terminator_tests =
  [ "llprograms/return.ll", 0L
  ; "llprograms/return42.ll", 42L
  ; "llprograms/br1.ll", 9L
  ; "llprograms/br2.ll", 17L    
  ; "llprograms/cbr1.ll", 7L
  ; "llprograms/cbr2.ll", 9L
  ; "llprograms/cbr3.ll", 9L
  ]

let bitcast_tests =
  [ "llprograms/bitcast1.ll", 3L
  ]

let gep_tests =
  [ "llprograms/gep1.ll", 6L
  ; "llprograms/gep2.ll", 4L
  ; "llprograms/gep3.ll", 1L
  ; "llprograms/gep4.ll", 2L
  ; "llprograms/gep5.ll", 4L
  ; "llprograms/gep6.ll", 7L
  ; "llprograms/gep7.ll", 7L    
  ; "llprograms/gep8.ll", 2L    
  ]

let io_tests =
  [ "llprograms/helloworld.ll", [], "hello, world!0"
  ; "llprograms/string1.ll", [], "hello, world!hello, world!0"
  ; "llprograms/callback1.ll", [], "380"
  ; "llprograms/args1.ll", ["hello"], "argc < 30"    
  ; "llprograms/args1.ll", ["hello"; "cis341"], "hellocis3410"
  ; "llprograms/args1.ll", ["hello"; "cis341"; "foo"], "argc > 30"    
  ]



let arithmetic_tests =
  [ "llprograms/add_twice.ll", 29L 
  ; "llprograms/sub_neg.ll", 255L (* Why, oh why, does the termianl only report the last byte? *)
  ; "llprograms/arith_combo.ll", 4L
  ; "llprograms/return_intermediate.ll", 18L ]

let sum_tree_tests = ["llprograms/sum_tree.ll", 116L]
let gcd_euclidian_tests = [ "llprograms/gcd_euclidian.ll", 2L]
let sieve_tests = [["cinterop.c"], "llprograms/sieve.ll", [], 1L]
let binary_search_tests = ["llprograms/binarysearch.ll", 8L]
let gep_5_deep_tests = ["llprograms/qtree.ll", 3L]
let binary_gcd_tests = ["llprograms/binary_gcd.ll", 3L]
let linear_search_tests = ["llprograms/linear_search.ll", 1L]
let lfsr_tests = ["llprograms/lfsr.ll", 108L]
let naive_factor_tests = 
  [ "llprograms/naive_factor_prime.ll", 1L
  ; "llprograms/naive_factor_nonprime.ll", 0L
  ]
let euclid_recursive_test = ["llprograms/euclid.ll", 2L]
let matmul_tests = ["llprograms/matmul.ll", 0L]

let large_tests = [ "llprograms/list1.ll", 3L
                  ; "llprograms/cbr.ll", 42L
                  ; "llprograms/factorial.ll", 120L
                  ; "llprograms/factrect.ll", 120L
                  ]

let ll_tests =
  binop_tests 
  @ terminator_tests 
  @ memory_tests 
  @ calling_convention_tests 
  @ bitcast_tests
  @ gep_tests 
  @ arithmetic_tests 
  @ sum_tree_tests
  @ gcd_euclidian_tests
  @ binary_search_tests
  @ gep_5_deep_tests
  @ binary_gcd_tests
  @ linear_search_tests
  @ lfsr_tests
  @ naive_factor_tests
  @ euclid_recursive_test
  @ matmul_tests
  @ large_tests 


let hw4_easiest_tests = [
  ("atprograms/easyrun1.oat", "", "17");
  ("atprograms/easyrun2.oat", "", "35");
  ("atprograms/easyrun3.oat", "", "73");
  ("atprograms/easyrun4.oat", "", "6");
  ("atprograms/easyrun5.oat", "", "212");
  ("atprograms/easyrun6.oat", "", "9");
  ("atprograms/easyrun7.oat", "", "23");
  ("atprograms/easyrun8.oat", "", "160");
  ("atprograms/easyrun9.oat", "", "236");
]

let hw4_globals_tests = [
  ("atprograms/globals1.oat", "", "42");
  ("atprograms/globals2.oat", "", "17");
  ("atprograms/globals3.oat", "", "17");
  ("atprograms/globals4.oat", "", "5");
  ("atprograms/globals5.oat", "", "17");
  ("atprograms/globals6.oat", "", "15");
]

let hw4_path_tests = [
 ("atprograms/path1.oat", "", "17");
 ("atprograms/path2.oat", "", "35");
 ("atprograms/path3.oat", "", "3");
 ("atprograms/arrayargs1.oat", "", "17");
 ("atprograms/arrayargs2.oat", "", "17");
 ("atprograms/arrayargs3.oat", "", "34");
]

let hw4_easy_tests = [
    ("atprograms/run26.oat", "", "0");
    ("atprograms/run27.oat", "", "99");
    ("atprograms/run28.oat", "", "18");
    ("atprograms/run29.oat", "", "1");
    ("atprograms/run30.oat", "", "9");
    ("atprograms/run31.oat", "", "9");
    ("atprograms/run13.oat", "", "1");
    ("atprograms/run32.oat", "", "33");
    ("atprograms/run21.oat", "", "99");
    ("atprograms/run33.oat", "", "1");
    ("atprograms/run34.oat", "", "66");
    ("atprograms/run35.oat", "", "66");
    ("atprograms/run38.oat", "", "31");
    ("atprograms/run39.oat", "a", "2");
    ("atprograms/run40.oat", "", "8");
    ("atprograms/run41.oat", "", "3");
    ("atprograms/run42.oat", "", "2");
    ("atprograms/run49.oat", "", "abc0");
    ("atprograms/run50.oat", "", "abcde0");
    ("atprograms/run60.oat", "", "85");
    ("atprograms/run61.oat", "", "3410");
]

let hw4_medium_tests = [
  ("atprograms/fact.oat", "", "1200");
  ("atprograms/run1.oat", "", "153");
  ("atprograms/run2.oat", "", "6");
  ("atprograms/run3.oat", "", "2");
  ("atprograms/run5.oat", "", "4");
  ("atprograms/run8.oat", "", "2");
  ("atprograms/run9.oat", "", "4");
  ("atprograms/run10.oat", "", "5");
  ("atprograms/run11.oat", "", "7");
  ("atprograms/run14.oat", "", "16");
  ("atprograms/run15.oat", "", "19");
  ("atprograms/run16.oat", "", "13");
  ("atprograms/run22.oat", "", "abc0");
  ("atprograms/run23.oat", "", "1230");
  ("atprograms/run25.oat", "", "nnn0");
  ("atprograms/run43.oat", "", "42");
  ("atprograms/run44.oat", "", "hello0");
  ("atprograms/run45.oat", "", "420");
  ("atprograms/run46.oat", "", "420");
  ("atprograms/run47.oat", "", "3");
  ("atprograms/run48.oat", "", "11");
  ("atprograms/lib4.oat", "", "53220");
  ("atprograms/lib5.oat", "", "20");
  ("atprograms/lib6.oat", "", "56553");
  ("atprograms/lib7.oat", "", "53");
  ("atprograms/lib8.oat", "", "Hello world!0");
  ("atprograms/lib9.oat", "a b c d", "abcd5");
  ("atprograms/lib11.oat", "", "45");
  ("atprograms/lib14.oat", "", "~}|{zyxwvu0");
  ("atprograms/lib15.oat", "123456789", "456780");
]

let hw4_hard_tests = [
("atprograms/fac.oat", "", "120");
("atprograms/qsort.oat", "", "kpyf{shomfhkmopsy{255");
("atprograms/bsort.oat", "", "y}xotnuw notuwxy}255");
("atprograms/msort.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("atprograms/msort2.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0");
("atprograms/selectionsort.oat", "", "01253065992000");
("atprograms/matrixmult.oat", "", "19 16 13 23 \t5 6 7 6 \t19 16 13 23 \t5 6 7 6 \t0");
]

let hw4_old_student_tests = [
    ("atprograms/binary_search.oat", "", "Correct!0")
  ; ("atprograms/xor_shift.oat", "", "838867572\n22817190600")
  ; ("atprograms/sieve.oat", "", "25")
  ; ("atprograms/count_sort.oat", "", "AFHZAAEYC\nAAACEFHYZ0")
  ; ("atprograms/determinant_size2.oat", "", "94")
  ; ("atprograms/fibo.oat", "", "0")
  ; ("atprograms/bubble_sort.oat", "", "1")
  ; ("atprograms/heap.oat", "", "1")
  ; ("atprograms/binary_gcd.oat", "", "3")
  ; ("atprograms/lfsr.oat", "", "TFTF FFTT0")
  ; ("atprograms/gnomesort.oat", "", "01253065992000")
  ; ("atprograms/josh_joyce_test.oat", "", "0")
  ; ("atprograms/conquest.oat", "", "My name is Jeff...\nCharizard is the BEST Pokemon ever!!!11")
  ; ("atprograms/gcd.oat", "", "16")
  ; ("atprograms/lcs.oat", "", "OAT0")
  ; ("atprograms/insertion_sort.oat", "", "42")
  ; ("atprograms/maxsubsequence.oat", "", "107")
]

let struct_tests = [
("hw5programs/compile_assign_struct.oat", "", "16");
("hw5programs/compile_basic_struct.oat", "", "7");
("hw5programs/compile_global_struct.oat", "", "254");
("hw5programs/compile_nested_struct.oat", "", "10");
("hw5programs/compile_return_struct.oat", "", "0");
("hw5programs/compile_struct_array.oat", "", "15");
("hw5programs/compile_struct_fptr.oat", "", "7");
("hw5programs/compile_various_fields.oat", "", "hello253"); 
]

let fptr_tests = [
  ("hw5programs/compile_array_fptr.oat", "", "2");
  ("hw5programs/compile_func_argument.oat", "", "4");
  ("hw5programs/compile_global_fptr.oat", "", "7");
  ("hw5programs/compile_global_fptr_unordered.oat", "", "2");
  ("hw5programs/compile_scall_fptr.oat", "", "4");
  ("hw5programs/compile_var_fptr.oat", "", "1");
  ("hw5programs/compile_local_fptr.oat", "", "5");
  ("hw5programs/compile_function_shadow.oat", "", "12");
  ("hw5programs/compile_global_struct_fptr.oat", "", "20");
  ("hw5programs/compile_builtin_argument.oat", "", "abab0");    
]

let regalloc_tests = [
  ("atprograms/regalloctest.oat", "", "0")
]

let oat_tests =
      hw4_easiest_tests
    @ hw4_globals_tests
    @ hw4_path_tests
    @ hw4_easy_tests
    @ hw4_medium_tests
    @ hw4_hard_tests
    @ hw4_old_student_tests
    @ struct_tests
    @ fptr_tests
    @ regalloc_tests

let tests : suite =
  [ 
   GradedTest("solver / alias analysis tests", 20, executed_alias_file alias_analysis_tests)
  ; GradedTest("liveness analysis tests", 10, executed_liveness_file liveness_analysis_tests)
  ; GradedTest("ll regalloc correctness tests", 15, executed ll_tests)  
  ; GradedTest("ll regalloc quality tests", 15, quality_ll ll_tests)
  ; GradedTest("oat regalloc correctness tests", 15, executed_oat_file oat_tests) 
  ; GradedTest("oat regalloc quality tests", 15, quality_oat oat_tests)
  ]

let manual_tests : suite = [
    GradedTest ("Posted Piazza Test Case", 5,
              [("manually", assert_eq true false)]
    )
  ; GradedTest ("Performance Comparison", 5,
              [("manually", assert_eq true false)]
    )
  ]

let graded_tests : suite =
  tests @
  manual_tests
