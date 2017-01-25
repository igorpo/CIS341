open Assert
open Simulator
open X86
open Asm

(* Test suite for asm.ml *)

(* Do NOT modify this file -- we will overwrite it with our *)
(* own version when we test your project.                   *)

(* These tests will be used to grade your assignment *)

(* Example Programs *)

let helloworld = [ text "foo"
                            [ Xorq, [~%Rax; ~%Rax]
                            ; Movq, [~$100; ~%Rax]
                            ; Retq, []
                            ]
                     ; text "main" 
                            [ Xorq, [~%Rax; ~%Rax]
                            ; Movq, [Ind1 (Lbl "baz"); ~%Rax]
                            ; Retq, []
                            ]
                     ; data "baz" 
                            [ Quad (Lit 99L)
                            ; Asciz "Hello, world!"
                            ]
                     ]

let factorial_iter n = [ text "main"
                                  [ Movq,  [~$1; ~%Rax]
                                  ; Movq,  [~$n; ~%Rdi]
                                  ]
                           ; text "loop"
                                  [ Cmpq,  [~$0; ~%Rdi]
                                  ; J Eq,  [~$$"exit"]
                                  ; Imulq, [~%Rdi; ~%Rax]
                                  ; Decq,  [~%Rdi]
                                  ; Jmp,   [~$$"loop"]
                                  ]
                           ; text "exit"
                                  [ Retq,  [] 
                                  ]
                           ]

let factorial_rec n = [ text "fac"
                                 [ Subq,  [~$8; ~%Rsp]
                                 ; Cmpq,  [~$1; ~%Rdi]
                                 ; J Le,  [~$$"exit"]
                                 ; Movq,  [~%Rdi; Ind2 Rsp]
                                 ; Decq,  [~%Rdi]
                                 ; Callq, [~$$"fac"]
                                 ; Imulq, [Ind2 Rsp; ~%Rax]
                                 ; Addq,  [~$8; ~%Rsp]
                                 ; Retq,  []
                                 ]
                          ; text "exit"
                                 [ Movq,  [~$1; ~%Rax]
                                 ; Addq,  [~$8; ~%Rsp]
                                 ; Retq,  [] 
                                 ]
                          ; gtext "main"
                                 [ Movq,  [~$n; ~%Rdi]
                                 ; Callq, [~$$"fac"]
                                 ; Retq,  []
                                 ]
                      ]

(* Object Builders *)

let sbyte_list (a: sbyte array) (start: int) : sbyte list =
  Array.to_list (Array.sub a start 8)

let stack_offset (i: quad) : operand = Ind3 (Lit i, Rsp)

let test_exec: exec =
  { entry = 0x400008L
  ; text_pos = 0x400000L
  ; data_pos = 0x400064L
  ; text_seg = [] 
  ; data_seg = []
  }

let test_machine (bs: sbyte list): mach =
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list bs) 0 mem 0 (List.length bs);
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- mem_bot;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }

let helloworld_dataseg =
  [ Byte 'c'; Byte '\x00'; Byte '\x00'; Byte '\x00'
  ; Byte '\x00'; Byte '\x00'; Byte '\x00'; Byte '\x00'
  ; Byte 'H'; Byte 'e' ; Byte 'l'; Byte 'l'
  ; Byte 'o'; Byte ','; Byte ' '; Byte 'w'
  ; Byte 'o'; Byte 'r'; Byte 'l'; Byte 'd'
  ; Byte '!'; Byte '\x00' ]

let helloworld_textseg =
  [ InsB0 (Xorq, [Reg Rax; Reg Rax]); InsFrag; InsFrag; InsFrag
  ; InsB0 (Movq, [Imm (Lit 100L); Reg Rax]); InsFrag; InsFrag; InsFrag
  ; InsB0 (Retq, []); InsFrag; InsFrag; InsFrag
  ; InsB0 (Xorq, [Reg Rax; Reg Rax]); InsFrag; InsFrag; InsFrag
  ; InsB0 (Movq, [Ind1 (Lit 0x400018L); Reg Rax]); InsFrag; InsFrag; InsFrag
  ; InsB0 (Retq, []); InsFrag; InsFrag; InsFrag
  ]

(* Testing Functions *)

let interp_cnd_test (fo, fs, fz) tru () =
  let flags = {fo = fo; fs = fs; fz = fz} in
  let all = [Eq;Neq;Gt;Ge;Lt;Le] in
  let fls = List.filter (fun c -> not (List.mem c tru)) all in
  let fn = interp_cnd flags in
  let tru' = List.filter fn all in
  let fls' = List.filter (fun c -> not (List.mem c tru')) all in
  List.iter (fun c ->
    if not (List.mem c tru)
    then failwith (Printf.sprintf "o:%b s:%b f:%b %s expected"
      fo fs fz (string_of_cnd c))
    else ()
  ) tru';
  List.iter (fun c ->
    if not (List.mem c fls)
    then failwith (Printf.sprintf "o:%b s:%b f:%b %s !expected"
      fo fs fz (string_of_cnd c))
    else ()
  ) fls'

let cc_test (s:string) (n: int) (m: mach) (fo', fs', fz') (f: mach -> bool) () =
  let m' = {m with flags = {fo=fo';fs=fs';fz=fz'}} in
  for i=1 to n do step m' done;
  if (f m') then () else failwith s

let cs_test (n:int) (m:mach) (fo',fs',fz') =
  cc_test (Printf.sprintf "expected OF:%b SF:%b ZF:%b" fo' fs' fz')
    n m (not fo',not fs',not fz')
    (fun m -> m.flags.fo = fo' && m.flags.fs = fs' && m.flags.fz = fz')
    
let cso_test (n: int) (m:mach) (fo':bool) =
  cc_test (Printf.sprintf "expected OF:%b" fo') n m (not fo',false,false)
    (fun m -> m.flags.fo = fo')

let csi_test (n: int) (m:mach) =
  cc_test "expected TTT ccodes" n m (true,true,true)
    (fun m -> m.flags.fo && m.flags.fs && m.flags.fz)

let segfault_test addr () =
  match map_addr addr with 
    | Some i -> failwith "Should have raised X86_segmentation_fault"
    | None -> ()

let undefinedsym_test (p:prog) () =
  try ignore (assemble p);
    failwith "Should have raised Undefined_sym"
  with 
    | Undefined_sym _ -> ()
    | _ -> failwith "Should have raised Undefined_sym"

let machine_test (s:string) (n: int) (m: mach) (f:mach -> bool) () =
  for i=1 to n do step m done;
  if (f m) then () else failwith ("expected " ^ s)

let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()

(* Tests *)

let map_addr_tests = [
    ("map_addr1", assert_eqf (fun () -> (map_addr 0x40FFF8L)) (Some 65528));
    ("map_addr2", assert_eqf (fun () -> (map_addr 0x4000FFL)) (Some 255));
    ("map_addr3", assert_eqf (fun () -> (map_addr 0x400000L)) (Some 0));
    ("map_addr4", segfault_test 0x0000000000000000L);
    ("map_addr5", segfault_test 0xFFFFFFFFFFFFFFFDL);
]

let interp_cnd_tests = [
    ("ccs_fff", interp_cnd_test (false,false,false) [Neq;Gt;Ge] );
    ("ccs_fft", interp_cnd_test (false,false,true)  [Eq;Le;Ge]  );
    ("ccs_ftf", interp_cnd_test (false,true,false)  [Neq;Le;Lt] );
    ("ccs_ftt", interp_cnd_test (false,true,true)   [Eq;Le;Lt]  );
    ("ccs_tff", interp_cnd_test (true,false,false)  [Neq;Le;Lt] );
    ("ccs_tft", interp_cnd_test (true,false,true)   [Eq;Le;Lt]  );
    ("ccs_ttf", interp_cnd_test (true,true,false)   [Neq;Gt;Ge] );
    ("ccs_ttt", interp_cnd_test (true,true,true)    [Eq;Le;Ge]  );
]

let mov_ri = test_machine
  [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag]


let add = test_machine
  [InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [~%Rbx; stack_offset 0L]);InsFrag;InsFrag;InsFrag
  ]



let functionality_tests = [
  ("mov_ri", machine_test "rax=42" 1 mov_ri
    (fun m -> m.regs.(rind Rax) = 42L)
  );

  ("add", machine_test "rax=rbx=*66528=1" 3 add
    (fun m -> m.regs.(rind Rax) = 1L
           && m.regs.(rind Rbx) = 1L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 1L
    )
  );

]

let mov_mr = test_machine
  [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~%Rax; stack_offset (-8L)]);InsFrag;InsFrag;InsFrag]


let subq = test_machine
    [InsB0 (Subq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Subq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Subq, [~%Rbx; stack_offset 0L]);InsFrag;InsFrag;InsFrag]

let andq = test_machine
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$3; ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$255; ~%Rcx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$1; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Andq, [~%Rax; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Andq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Andq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Andq, [stack_offset 0L; ~%Rcx]);InsFrag;InsFrag;InsFrag
    ]


let negq = test_machine
    [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$(-24); stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [Imm (Lit Int64.min_int); ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [~%Rbx]);InsFrag;InsFrag;InsFrag
    ]


let shl = test_machine
    [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$2; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$3; ~%Rcx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Shlq, [~%Rcx; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ]


let imul = test_machine
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$22; ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Imulq, [~%Rbx; ~%Rax]);InsFrag;InsFrag;InsFrag
    ]

let pushq = test_machine
    [InsB0 (Pushq, [~$42]);InsFrag;InsFrag;InsFrag]

let popq = test_machine
    [InsB0 (Addq, [~$(-8); ~%Rsp]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$42; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Popq, [~%Rax]);InsFrag;InsFrag;InsFrag
    ]

let cmpq = test_machine
    [InsB0 (Movq, [~$4; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$2; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~%Rax; ~%Rbx]);InsFrag;InsFrag;InsFrag
    ;InsB0 (Cmpq, [~%Rbx; stack_offset 0L]);InsFrag;InsFrag;InsFrag
    ]

let instruction_tests = [
  ("mov_mr", machine_test "*65520=42" 2 mov_mr
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L)
  );
 
  ("subq", machine_test "rax=*65528=-1L; rbx=1" 3 subq
    (fun m -> m.regs.(rind Rax) = (Int64.neg 1L)
           && m.regs.(rind Rbx) = 1L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = (Int64.neg 1L)
    )
  );
  ("andq", machine_test "rax=2 rbx=2 rcx=1 *65528=1" 8 andq
    (fun m -> m.regs.(rind Rax) = 2L
           && m.regs.(rind Rbx) = 2L
           && m.regs.(rind Rcx) = 1L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 1L
    )
  );

  ("negq", machine_test "rax=-42 rbx=min_int64 *65528=24" 6 negq
    (fun m -> m.regs.(rind Rax) = Int64.neg 42L
           && m.regs.(rind Rbx) = Int64.min_int
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 24L
    )
  );

  ("shl", machine_test "rax=4 *65528=16" 5 shl
    (fun m -> m.regs.(rind Rax) = 4L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 16L
    )
  );

  ("imul", machine_test "rax=44 *65528=2" 3 imul
    (fun m -> m.regs.(rind Rax) = 44L)
  );
  ("pushq", machine_test "rsp=4 *65520=2A" 1 pushq
    (fun m -> m.regs.(rind Rsp) = 0x0040FFF0L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 0x2AL
    )
  );
  ("popq", machine_test "rsp=4259832 rax=2A" 3 popq
    (fun m -> m.regs.(rind Rax) = 0x2AL
           && m.regs.(rind Rsp) = 0x0040FFF8L
    )
  );
  ("cmpq", machine_test "rax=4 rbx=0" 5 cmpq
    (fun m -> m.regs.(rind Rax) = 4L
           && m.regs.(rind Rbx) = 0L
           && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 2L
    )
  );
]

let cc_add_1 = test_machine
  [InsB0 (Movq, [Imm (Lit 0xFFFFFFFFFFFFFFFFL); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let cc_add_2 = test_machine
  [InsB0 (Movq, [Imm (Lit 0xFFFFFFFFFFFFFFFFL); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [Imm (Lit 0xFFFFFFFFFFFFFFFFL); ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let cc_add_3 = test_machine
  [InsB0 (Movq, [Imm (Lit 0x7FFFFFFFFFFFFFFFL); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let cc_add_4 = test_machine
  [InsB0 (Movq, [Imm (Lit 0x9000000000000000L); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Addq, [Imm (Lit 0xA000000000000000L); ~%Rax]);InsFrag;InsFrag;InsFrag

  ]

let cc_neg_1 = test_machine
  [InsB0 (Movq, [Imm (Lit Int64.min_int); ~%Rbx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Negq, [~%Rbx]);InsFrag;InsFrag;InsFrag
  ]

let cc_neg_2 = test_machine
  [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Negq, [~%Rax]);InsFrag;InsFrag;InsFrag
  ]


let cc_cmp_1 = test_machine
  [InsB0 (Movq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Cmpq, [Imm (Lit 0x8000000000000000L); ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let cc_cmp_2 = test_machine
  [InsB0 (Movq, [Imm (Lit 0x8000000000000000L); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Cmpq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag
  ]


let cc_imul_1 = test_machine
  [InsB0 (Movq, [~$(-1); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Imulq, [~$(-1); ~%Rax]);InsFrag;InsFrag;InsFrag
  ]


let cc_and = test_machine
  [InsB0 (Movq, [Imm (Lit 0x0F0F0F0FL); ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Andq, [Imm (Lit 0xF0F0F0F0L); ~%Rax]);InsFrag;InsFrag;InsFrag
  ]

let cc_or = test_machine
  [InsB0 (Movq, [~$0xFFFFFFF; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Orq, [~$0xF0F0F0F0; ~%Rax]);InsFrag;InsFrag;InsFrag
  ]


let cc_set = test_machine
  [InsB0 (Set Neq, [~%Rax]);InsFrag;InsFrag;InsFrag]

let cc_push = test_machine
  [InsB0 (Pushq, [~$0]);InsFrag;InsFrag;InsFrag]

let cc_pop = test_machine
  [InsB0 (Popq, [~%Rax]);InsFrag;InsFrag;InsFrag]

let cc_ret = test_machine
  [InsB0 (Retq, []);InsFrag;InsFrag;InsFrag]

let cc_mov = test_machine
  [InsB0 (Movq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag]

let cc_jmp = test_machine
  [InsB0 (Jmp, [~$0x400008]);InsFrag;InsFrag;InsFrag]

let cc_js = test_machine
  [InsB0 (J Neq, [~$0x400008]);InsFrag;InsFrag;InsFrag]

let cc_jf = test_machine
  [InsB0 (J Eq, [~$0x400008]);InsFrag;InsFrag;InsFrag]

let cc_call = test_machine
  [InsB0 (Callq, [~$0x400008]);InsFrag;InsFrag;InsFrag]

let cc_lea = test_machine
  [InsB0 (Movq, [~$0x400600; ~%Rax]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Movq, [~$0x408000; ~%Rcx]);InsFrag;InsFrag;InsFrag
  ;InsB0 (Leaq, [Ind2 Rax; Ind2 Rcx]);InsFrag;InsFrag;InsFrag]

let condition_flag_set_tests =
  [ ("cc_add_1", cs_test 2 cc_add_1 (false, false, true))
  ; ("cc_add_2", cs_test 2 cc_add_2 (false, true, false))
  ; ("cc_add_3", cs_test 2 cc_add_3 (true, true, false))
  ; ("cc_add_4", cs_test 2 cc_add_4 (true, false, false))
  ; ("cc_neg_1", cs_test 2 cc_neg_1 (true, true, false))
  ; ("cc_neg_2", cs_test 2 cc_neg_2 (false, true, false))

  ; ("cc_cmp_1", cs_test 2 cc_cmp_1 (true, true, false))
  ; ("cc_cmp_2", cs_test 2 cc_cmp_2 (false, true, false))

  ; ("cc_imul_1", cso_test 2 cc_imul_1 false)

  ; ("cc_and", cs_test 2 cc_and (false, false, true))
  ; ("cc_or", cs_test 2 cc_or (false, false, false))

  ; ("cc_push", csi_test 1 cc_push)
  ; ("cc_pop", csi_test 1 cc_pop)
  ; ("cc_set", csi_test 1 cc_set)
  ; ("cc_ret", csi_test 1 cc_ret)
  ; ("cc_mov", csi_test 1 cc_mov)
  ; ("cc_jmp", csi_test 1 cc_jmp)
  ; ("cc_jmp", csi_test 1 cc_js)
  ; ("cc_jmp", csi_test 1 cc_jf)
  ; ("cc_call", csi_test 1 cc_call)
  ; ("cc_lea", csi_test 3 cc_lea)
  ]

(* Test Suites *)

let easy_tests : suite =
[
  GradedTest("Map Addresses", 5, map_addr_tests);
  GradedTest("Condition Codes", 5, interp_cnd_tests);
  GradedTest("Easy Assemble Tests", 5,[
    ("assemble1", assert_eqf (fun () -> (assemble helloworld).text_pos) 0x400000L );
    ("assemble2", assert_eqf (fun () -> (assemble helloworld).data_pos) 0x400018L );
  ]);
  GradedTest("Easy Load Tests", 5,[
    ("load_flags", assert_eqf (fun () -> (load test_exec).flags)
                              {fo = false; fs = false; fz = false});
    ("load_rip", assert_eqf (fun () -> (load test_exec).regs.(rind Rip))
                             0x400008L);
    ("load_rsp", assert_eqf (fun () -> (load test_exec).regs.(rind Rsp))
                             0x40FFF8L);
  ]);
]

let medium_tests : suite = [
  GradedTest("Medium Assemble Tests", 5,[
    ("assemble1", assert_eqf (fun () -> (assemble helloworld).text_seg) helloworld_textseg );
    ("assemble2", undefinedsym_test [text "foo" [Retq,[]]]);
    
  ]);
  GradedTest("Medium Load Tests", 5,[
    ("load_exit_addr", assert_eqf (fun () ->
          let m = load test_exec in
          int64_of_sbytes (sbyte_list m.mem 0x0fff8)
    ) exit_addr);
  ]);
  GradedTest("Functionality Tests", 5, functionality_tests);
  GradedTest("Instruction Tests", 10, instruction_tests);
  GradedTest("Condition Flag Set Tests", 5, condition_flag_set_tests);
]

let hard_tests : suite = [
  GradedTest ("Factorial", 10, [
    ("fact6", program_test (factorial_rec 6) 720L);
  ]);
  GradedTest ("Hard", 10, []);
] 

let manual_tests : suite = [
  GradedTest ("PartIIITestCase (manual)", 15, [
  
  ]);
  GradedTest ("Style (manual)", 5, [
  
  ]);
]

let graded_tests : suite =
  easy_tests @
  medium_tests @
  hard_tests @
  manual_tests
