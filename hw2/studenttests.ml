open Simulator
open Assert
open X86
open Asm

(* copied from gradedtests.ml *)
let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()

(* Begin student tests *)
let asgoel_tests =
    let fib n =
      let rec loop one_back two_back count = if count >= n
        then one_back
        else loop (one_back + two_back) one_back (count + 1)
      in loop 1 0 0
    in

    let fibs_asm n =
      let one_back = ~%R10 in
      let two_back = ~%R11 in
      let i = ~%R12 in
      let fib_n = ~%R13 in
      [ text "fib" [ 
            Movq, [one_back; ~%Rax]
          ; Addq, [two_back; ~%Rax]
          ; Movq, [one_back; two_back]
          ; Movq, [~%Rax   ; one_back]
          ; Addq, [~$1     ; i]
          ; Cmpq, [fib_n   ; i]
          ; J Lt, [~$$"fib"]
          ; Retq, []
          ]
      ; gtext "main"
          [ Movq, [~$1      ; one_back] (*one_back*)
          ; Movq, [~$0      ; two_back] (*two_back*)
          ; Movq, [~$1      ; i] (*current index in sequence*)
          ; Movq, [~$(n + 1); fib_n] (*cap for R12*)
          ; Callq,[~$$"fib"] 
          ; Retq, []
          ]
      ]
    in

    List.map (fun fib_n -> ("fib "^(string_of_int fib_n),
      program_test (fibs_asm fib_n) (Int64.of_int @@ fib fib_n)
    )) [1;2;3;4;5;6;7;8;9;10]

let cbarcen_tests =
    let mod_program a b =
      [ text "main"
          [ Movq, [~$a; ~%Rax]
          ; Movq, [~$b; ~%Rbx]
          ; Callq, [~$$"mod"]
          ; Retq, []
          ]
      ; text "mod"
          [ Cmpq, [~%Rbx; ~%Rax]
          ; J Lt, [~$$"mod_return"]
          ; Subq, [~%Rbx; ~%Rax]
          ; Jmp,  [~$$"mod"]
          ]
      ; text "mod_return"
          [ Retq, [] ]
      ]
    in
    let gcd_program a b : prog =
      [ text "main"
          [ Movq,  [~$a; ~%Rax]
          ; Movq,  [~$b; ~%Rbx]
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
      ; text "gcd"
          [ Cmpq,  [~$0; ~%Rbx]
          ; J Le,  [~$$"return"]
          ; Pushq, [~%Rbx]
          ; Callq, [~$$"mod"]
          ; Movq,  [~%Rax; ~%Rbx]
          ; Popq,  [~%Rax]
          ; Callq, [~$$"gcd"]
          ; Retq,  []
          ]
      ; text "mod"
          [ Cmpq, [~%Rbx; ~%Rax]
          ; J Lt, [~$$"return"]
          ; Subq, [~%Rbx; ~%Rax]
          ; Jmp,  [~$$"mod"]
          ]
      ; text "return"
          [ Retq, [] ]
      ]
    in

    [ (* Mod tests *)
      ("10 % 3 = 1", program_test (mod_program 10 3) 1L);
      ("10 % 5 = 0", program_test (mod_program 10 5) 0L);
      ("37 % 35 = 2", program_test (mod_program 37 35) 2L);
      ("51251 % 9999 = 1256", program_test (mod_program 51251 9999) 1256L);
      ("35 % 35 = 0", program_test (mod_program 35 35) 0L);
      (* Recursive Euclidean GCD tests *)
      ("gcd (10, 10) = 10", program_test (gcd_program 10 10) 10L);
      ("gcd (5220, 75) = 15", program_test (gcd_program 5220 75) 15L);
      ("gcd (179, 64) = 1", program_test (gcd_program 179 64) 1L);
      ("gcd (731, 43) = 43", program_test (gcd_program 731 43) 43L);
      ("gcd (3253, 3323) = 1", program_test (gcd_program 3253 3323) 1L);
      ("gcd (72, 1) = 1", program_test (gcd_program 72 1) 1L);
    ]

let charcobb_tests =
  let gcd a b = [
    text "gcd"
    [ Cmpq, [~%Rsi; ~%Rdi]
    ; J Eq, [~$$"exit"]
    ; J Le, [~$$"gcd_else"]
    ; Subq, [~%Rsi; ~%Rdi]
    ; Jmp, [~$$"gcd"]
    ];
    text "gcd_else"
    [ Subq, [~%Rdi; ~%Rsi]
    ; Jmp, [~$$"gcd"]
    ];
    text "exit"
    [ Movq, [~%Rdi; ~%Rax]
    ; Retq, []
    ];
    gtext "main"
    [ Movq, [~$a; ~%Rdi]
    ; Movq, [~$b; ~%Rsi]
    ; Callq, [~$$"gcd"]
    ; Retq, []
    ]
  ] in
  
  let log n = [
    text "log"
    [ Movq, [~$0; ~%Rax]
    ; Movq, [~$0; ~%Rsi]
    ; Cmpq, [~$0; ~%Rdi]
    ; J Le, [~$$"exit"]
    ];
    text "loop1"
    [ Incq, [~%Rsi]
    ; Shrq, [~$1; ~%Rdi]
    ; Cmpq, [~$0; ~%Rdi]
    ; J Neq, [~$$"loop1"]
    ; Movq, [~$1; ~%Rdi]
    ; Subq, [~$1; ~%Rsi]
    ; Movq, [~%Rsi; ~%Rcx]
    ; Shlq, [~%Rcx; ~%Rdi]
    ];
    text "loop2"
    [ Incq, [~%Rax]
    ; Shlq, [~$1; ~%Rdi]
    ; J Gt, [~$$"loop2"]
    ];
    text "exit"
    [ Subq, [~$64; ~%Rax]
    ; Imulq, [~$(-1); ~%Rax]
    ; Retq, []
    ];
    gtext "main"
    [ Movq, [Imm (Lit n); ~%Rdi]
    ; Callq, [~$$"log"]
    ; Retq, []
    ];
  ] in

  [   (* GCD of two positive integers. *)
      ("gcd 10 20", program_test (gcd 10 20) 10L);
      ("gcd 20 20", program_test (gcd 20 20) 20L);
      ("gcd 18 12", program_test (gcd 18 12) 6L);
      ("gcd 23 19", program_test (gcd 23 19) 1L);
      (* Floor of the logarithm (base 2) of a positive integer. *)
      ("log 1", program_test (log 1L) (0L));
      ("log 2", program_test (log 2L) (1L));
      ("log 3", program_test (log 3L) (1L));
      ("log 17", program_test (log 17L) (4L));
      ("log 23", program_test (log 23L) (4L));
      ("log 24", program_test (log 24L) (4L));
      ("log 31", program_test (log 31L) (4L));
      ("log 2^63-1", program_test (log 0x7fffffffffffffffL) (62L));
  ]

let chinz_tests =
  let heapsort (l:int64 list) =
    let len = List.length l in [
      text "main"
        [ Movq,  [~$$"array"; ~%R08]
        ; Movq,  [~$len; ~%R09]
        ; Callq, [~$$"heapify"]
        ]
    ; text "main_loop"
        [ Cmpq,  [~$1; ~%R09]
        ; J Lt,  [~$$"return"]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%R09; ~%R13]
        ; Decq,  [~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R15]
        ; Movq,  [Ind2 R08; Ind2 Rsi]
        ; Movq,  [~%R15; Ind2 R08]
        ; Decq,  [~%R09]
        ; Movq,  [~$0; ~%Rax]
        ; Callq, [~$$"siftdown"]
        ; Jmp,   [~$$"main_loop"]
        ; Retq,  []
        ]
    ; text "heapify"
        [ Movq,  [~%R09; ~%R10]
        ; Decq,  [~%R10]
        ]
    ; text "heapify_loop"
        [ Cmpq,  [~$0; ~%R10]
        ; J Lt,  [~$$"return"]
        ; Movq,  [~%R10; ~%Rax]
        ; Callq, [~$$"siftdown"]
        ; Decq,  [~%R10]
        ; Jmp,   [~$$"heapify_loop"]
        ]
    ; text "return"
        [ Retq, []
        ]
    ; text "siftdown"
        [ Movq,  [~%Rax; ~%Rbx]
        ; Shlq,  [~$1; ~%Rbx]
        ; Incq,  [~%Rbx]
        ; Cmpq,  [~%R09; ~%Rbx]
        ; J Ge,  [~$$"return"]
        ; Movq,  [~%Rax; ~%Rcx]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%Rcx; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R11]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%Rbx; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R12]
        ; Cmpq,  [~%R12; ~%R11]
        ; J Gt,  [~$$"not_left_child"]
        ; Movq,  [~%Rbx; ~%Rcx]
        ]
    ; text "not_left_child"
        [ Incq,  [~%Rbx]
        ; Xorq,  [~%R15; ~%R15]
        ; Xorq,  [~%R14; ~%R14]
        ; Cmpq,  [~%R09; ~%Rbx]
        ; Set Ge,[~%R15]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%Rbx; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R11]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%Rcx; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R12]
        ; Cmpq,  [~%R12; ~%R11]
        ; Set Le,[~%R14]
        ; Orq,  [~%R14; ~%R15]
        ; Cmpq,  [~$1; ~%R15]
        ; J Eq, [~$$"not_right_child"]
        ; Movq,  [~%Rbx; ~%Rcx]
        ]
    ; text "not_right_child"
        [ Cmpq,  [~%Rcx; ~%Rax]
        ; J Eq,  [~$$"return"]
        ; Movq,  [~%R08; ~%Rsi]
        ; Movq,  [~%Rax; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rsi]
        ; Movq,  [Ind2 Rsi; ~%R11]
        ; Movq,  [~%R08; ~%Rdi]
        ; Movq,  [~%Rcx; ~%R13]
        ; Shlq,  [~$3; ~%R13]
        ; Addq,  [~%R13; ~%Rdi]
        ; Movq,  [Ind2 Rdi; Ind2 Rsi]
        ; Movq,  [~%R11; Ind2 Rdi]
        ; Movq,  [~%Rcx; ~%Rax]
        ; Callq, [~$$"siftdown"]
        ; Retq,  []
        ]
    ; data "array"
        (List.map (fun n -> Quad (Lit n)) l)
    ]
  in
  (* obtains a list from a subarray of an array *)
  let rec array_slice (arr:'a array) (index:int) (size:int):'a list = 
    if size <= 0 then
      [] 
    else 
      arr.(index) :: (array_slice arr (index + 1) (size - 1)) in
  (* reads a quad from machine memory *)
  let get_quad_from_mem (addr:quad) (m:mach) : quad =
    begin match map_addr addr with
    | Some x -> int64_of_sbytes (array_slice m.mem x 8)
    | None -> raise X86lite_segfault
    end in
  (* Checks if sorted *)
  let rec sorted (l:int64 list) : bool =
    begin match l with
    | [] -> true
    | [x] -> true
    | x::(y::xs) -> ((Int64.compare x y) <= 0) && (sorted (y::xs))
    end in
  (* Gets the list of quads pointed to by R08 *)
  let extract_array (m:mach) (len:int) : quad list =
    let base = m.regs.(rind R08) in
    let rec f (addr:quad) (len:int) : quad list =
      if len = 0 then 
        []
      else 
        (get_quad_from_mem addr m) :: (f (Int64.add addr 8L) (len-1))
      in
    f base len in
  (* Runs the machine until its end state *)
  let run_mach (m:mach) : mach = 
    while m.regs.(rind Rip) <> exit_addr do step m done;
    m in 
  (* Tests if the end result is sorted *)
  let sort_test (l:quad list) : bool =
    let len = List.length l in
    let m = load (assemble (heapsort l)) in
    let m_done = run_mach m in
    let sorted1 = extract_array m_done len in
    sorted sorted1 in
  let array1 = [4L;2L;1L;5L;9L;8L;-5L;7L;2L;1338L;0L] in
  let array2 = [1259L;1249014L;19250125L;32L;-125L;353L;0L;0L;19L] in
  let array3 = [0L;1L;-1L;2L;-2L;3L;-3L;4L;-4L;5L;-5L;6L;-6L;7L] in
  let array4 = [10L;9L;8L;7L;6L;6L;5L;5L;5L;4L;3L;3L;2L;1L] in

  [ ("heapsort1", assert_eqf (fun () -> (sort_test array1)) true); 
    ("heapsort2", assert_eqf (fun () -> (sort_test array2)) true);
    ("heapsort3", assert_eqf (fun () -> (sort_test array3)) true);
    ("heapsort4", assert_eqf (fun () -> (sort_test array4)) true);
  ]

let dmally_tests =
  let euclid_test a b = [
      text "euclid"
          [ Cmpq,  [~%R08; ~%R09]
          ; J Eq,  [~$$"epilogue"]
          ; Cmpq,  [~%R08; ~%R09]
          ; J Lt,  [~$$"BLessThanA"]
          ; Subq,  [~%R08; ~%R09]
          ; Jmp,   [~$$"euclid"]
          ]
      ; text "BLessThanA"
          [ Subq,  [~%R09; ~%R08]
          ; Jmp,   [~$$"euclid"]
          ]
      ; text "epilogue"
          [ Movq,  [~%R08; ~%Rax]
          ; Retq,  []
          ]
      ; gtext "main"
          [ Movq,  [~$a; ~%R08]
          ; Movq,  [~$b; ~%R09]
          ; Callq, [~$$"euclid"]
          ; Retq,  []
          ]
  ] in

  [ ("Euclid 21 14", program_test (euclid_test 21 14) 7L);
    ("Euclid 14 27", program_test (euclid_test 14 21) 7L);
    ("Euclid 8 2", program_test (euclid_test 2 8) 2L);
    ("Euclid 2 8", program_test (euclid_test 8 2) 2L);
    ("Euclid 31 17", program_test (euclid_test 31 17) 1L);
  ]

let ellisl_tests =
(* Square root program
   B = sqrt(A); the largest integer such that B*B <= A
   R08 = A, R09 = B *)
  let sqrt n =
    [ text "cond"
        [ Cmpq, [~%R08; ~$0]
        ; J Gt, [~$$"exit"]
        ]
    ; text "loop"
        [ Movq, [~%R09; ~%R10]
        ; Imulq, [~%R10; ~%R10]
        ; Cmpq, [~%R10; ~%R08]
        ; J Lt, [~$$"exit"]
        ; Incq, [~%R09]
        ; Jmp, [~$$"loop"]
        ]
    ; text "exit"
        [ Decq, [~%R09]
        ; Movq, [~%R09; ~%Rax]
        ; Retq, []
        ]
    ; gtext "main"
        [ Movq,  [~$n; ~%R08]
        ; Movq, [~$0; ~%R09]
        ; Jmp, [~$$"cond"]
        ]
    ]
  in

  [ ("sqrt(0) = 0", program_test (sqrt 0) 0L);
    ("sqrt(1) = 1", program_test (sqrt 1) 1L);
    ("sqrt(2) = 2", program_test (sqrt 2) 1L);
    ("sqrt(3) = 2", program_test (sqrt 3) 1L);
    ("sqrt(4) = 2", program_test (sqrt 4) 2L);
    ("sqrt(36) = 6", program_test (sqrt 36) 6L);
  ]

let honki_tests =
  (* Computes the length of the collatz sequence starting at n
   * Valid inputs are positive and non-zero *)
  let collatz n =
    let rec loop (n:int) (steps:int) : int =
      begin match n with
        | 1 -> steps
        | n when n < 1 -> -1 (* Error *)
        | _ ->
            begin match n mod 2 with
              | 0 -> loop (n/2) (steps + 1)
              | 1 -> loop (3*n + 1) (steps + 1)
              | _ -> failwith "mod by 2 gives 0 or 1"
            end
      end in
      loop n 0
  in
  
  let collatz_prog n =
    [ text "collatz"
        [ Cmpq,  [~$1; ~%Rdi] (* n == 1 *)
        ; J Eq,  [~$$"exit"]
        ; J Lt,  [~$$"error"]
        ; Incq,  [~%Rax] (* steps++ *)
        ; Movq,  [~%Rdi; ~%R13] (* copy n *)
        ; Andq,  [~$1; ~%R13]
        ; Cmpq,  [~$1; ~%R13] (* n mod 2 == 1 *)
        ; J Eq,  [~$$"odd"]
        ; Sarq,  [~$1; ~%Rdi] (* n = n / 2 *)
        ; Callq, [~$$"collatz"]
        ; Retq,  []
        ]
    ; text "odd"
        [ Imulq, [~$3; ~%Rdi]
        ; Addq, [~$1; ~%Rdi] (* n = 3n + 1 *)
        ; Callq, [~$$"collatz"]
        ; Retq,  []
        ]
    ; text "error"
        [ Movq, [~$(-1); ~%Rax]
        ; Retq,  []
        ]
    ; text "exit"
        [ Retq,  []
        ]
    ; gtext "main"
        [ Movq,  [~$n; ~%Rdi]
        ; Movq,  [~$0; ~%Rax]
        ; Callq, [~$$"collatz"]
        ; Retq,  []
        ]
    ]
  in
  
  [ ("collatz ocaml -1", assert_eq (collatz (-2)) (-1));
    ("collatz ocaml 1", assert_eq (collatz 1) 0);
    ("collatz ocaml 16", assert_eq (collatz 16) 4); (* 16, 8, 4, 2, 1 *)
    ("collatz ocaml 17", assert_eq (collatz 15) 17); (* 15, 46, 23, 70,
                                                      * 35, 106, 53, 160,
                                                      * 80, 40, 20, 10, 5,
                                                      * 16, 8, 4, 2, 1 *)
    ("collatz assembly -1", program_test (collatz_prog (-2)) (-1L));
    ("collatz assembly 1",  program_test (collatz_prog 1) 0L);
    ("collatz assembly 16", program_test (collatz_prog 16) 4L);
    ("collatz assembly 17", program_test (collatz_prog 15) 17L)
  ]

let isibner_tests =
  let gcd_euclidean_algorithm a b =
    [ text "main"
             [ Movq, [~$a; ~%Rdi]
             ; Movq, [~$b; ~%Rsi]
             ; Callq, [~$$"gcd"]
             ; Retq, []
             ]
    ; text "gcd"
             [ Cmpq, [~$0; ~%Rdi] (* if a is 0, return b*)
             ; J Eq, [~$$"ret_b"]
             ]
    ; text "loop"
             [ Cmpq, [~$0; ~%Rsi] (* if b is 0, jump to return *)
             ; J Eq, [~$$"ret_a"]
             ; Cmpq, [~%Rsi; ~%Rdi] (* if a > b, a = a - b *)
             ; J Le, [~$$"else"]
             ]
    ; text "if"
             [ Movq, [~%Rsi; ~%R09]  (* R09 = -b *)
             ; Imulq, [~$(-1); ~%R09]
             ; Addq, [~%R09; ~%Rdi]  (* a = a + (-b) *)
             ; Jmp, [~$$"loop"]
             ]
    ; text "else"
             [ Movq, [~%Rdi; ~%R09] (* R09 = -a *)
             ; Imulq, [~$(-1); ~%R09]
             ; Addq, [~%R09; ~%Rsi]   (* b = b + (-a) *)
             ; Jmp, [~$$"loop"]
             ]
    ; text "ret_b"
             [ Movq, [~%Rsi; ~%Rax]
             ; Retq, []
             ]
    ; text "ret_a"
             [ Movq, [~%Rdi; ~%Rax]
             ; Retq, []
             ]
    ]
  in
  
  let gcd_steins_algorithm a b = 
    [ text "main"
          [ Movq, [~$a; ~%Rdi]
          ; Movq, [~$b; ~%Rsi]
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
    ; text "gcd"
          [ Cmpq, [~$0; ~%Rdi] (* if a == 0, return b *)
          ; J Eq, [~$$"ret_b"]
          ; Cmpq, [~$0; ~%Rsi] (* if b == 0, return a *)
          ; J Eq, [~$$"ret_a"]
  
          ; Movq, [~%Rdi; ~%R09] (* R09 = Rdi & 1 *)
          ; Andq, [~$1; ~%R09]
          ; Movq, [~%Rsi; ~%R10] (* R10 = Rsi & 1 *)
          ; Andq, [~$1; ~%R10] 
          ; Cmpq, [~%R09; ~%R10]
          ; J Eq, [~$$"equal"]
          ]
    ; text "not_equal"
          [ Cmpq, [~$0; ~%R09]
          ; J Eq, [~$$"ret_a_shift"]
          ; Cmpq, [~$0; ~%R10]
          ; J Eq, [~$$"ret_b_shift"]
          ]
    ; text "equal"
          [ Cmpq, [~$0; ~%R09] (* R09 == R10, so we only have to compare one of them *)
          ; J Eq, [~$$"shift_when_even"]
          ]
    ; text "shift_when_odd"
          [ Cmpq, [~%Rsi; ~%Rdi]
          ; J Ge, [~$$"a_geq_b_odd"]
          ; Jmp, [~$$"a_lt_b_odd"]
          ]
    ; text "shift_when_even"
          [ Shrq, [~$1; ~%Rdi]
          ; Shrq, [~$1; ~%Rsi]
          ; Callq, [~$$"gcd"]
          ; Shlq, [~$1; ~%Rax]
          ; Retq, []
          ]
    ; text "ret_a"
          [ Movq, [~%Rdi; ~%Rax]
          ; Retq, []
          ]
    ; text "ret_b"
          [ Movq, [~%Rsi; ~%Rax]
          ; Retq, []
          ]
    ; text "ret_a_shift"
          [ Shrq, [~$1; ~%Rdi]
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
    ; text "ret_b_shift"
          [ Shrq, [~$1; ~%Rsi]
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
    ; text "a_geq_b_odd"
          [ Movq, [~%Rsi; ~%R11] (* R11 = b *)
          ; Imulq, [~$(-1); ~%R11] (* R11 = -b *)
          ; Addq, [~%R11; ~%Rdi] (* a = -b + a *)
          ; Shrq, [~$1; ~%Rdi] (* a >>= 1 *)
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
    ; text "a_lt_b_odd"
          [ Movq, [~%Rdi; ~%R11] (* R11 = a *)
          ; Imulq, [~$(-1); ~%R11] (* R11 = -a *)
          ; Addq, [~%R11; ~%Rsi] (* b = -a + b *)
          ; Shrq, [~$1; ~%Rsi]  (* b >>= 1 *)
          ; Callq, [~$$"gcd"]
          ; Retq, []
          ]
    ] in
  
    [ ("GCD test a = b", program_test (gcd_euclidean_algorithm 5 5) 5L);
      ("GCD test a = 0", program_test (gcd_euclidean_algorithm 0 15) 15L);
      ("GCD test b = 0", program_test (gcd_euclidean_algorithm 15 0) 15L);
      ("GCD test a, b are prime", program_test (gcd_euclidean_algorithm 3 7) 1L);
      ("GCD test a, b are arbitrary", program_test (gcd_euclidean_algorithm 10 15) 5L);
      ("GCD test a = b", program_test (gcd_steins_algorithm 5 5) 5L);
      ("GCD test a = 0", program_test (gcd_steins_algorithm 0 15) 15L);
      ("GCD test b = 0", program_test (gcd_steins_algorithm 15 0) 15L);
      ("GCD test a, b are prime", program_test (gcd_steins_algorithm 3 7) 1L);
      ("GCD test a, b are arbitrary", program_test (gcd_steins_algorithm 10 15) 5L);
      ("GCD_euclidean_algorithm test a = b", program_test (gcd_euclidean_algorithm 5 5) 5L);
      ("GCD_euclidean_algorithm test a = 0", program_test (gcd_euclidean_algorithm 0 15) 15L);
      ("GCD_euclidean_algorithm test b = 0", program_test (gcd_euclidean_algorithm 15 0) 15L);
      ("GCD_euclidean_algorithm test a, b are prime", program_test (gcd_euclidean_algorithm 3 7) 1L);
      ("GCD_euclidean_algorithm test a, b are arbitrary", program_test (gcd_euclidean_algorithm 10 15) 5L);
      ("GCD_steins_algorithm test a = b", program_test (gcd_steins_algorithm 5 5) 5L);
      ("GCD_steins_algorithm test a = 0", program_test (gcd_steins_algorithm 0 15) 15L);
      ("GCD_steins_algorithm test b = 0", program_test (gcd_steins_algorithm 15 0) 15L);
      ("GCD_steins_algorithm test a, b are prime", program_test (gcd_steins_algorithm 3 7) 1L);
      ("GCD_steins_algorithm test a, b are arbitrary", program_test (gcd_steins_algorithm 10 15) 5L);
    ]

let jampa_tests =
(* OVERVIEW OF BINARY_SEARCH_TEST
This assembly program:
  - initializes a hardcoded list of 20 values starting at address 0x408000L.
  - runs binary search given a width (number of elements - 1) and target.
  - returns the NUMBER OF CALLS TO "binary_search" needed to find target,
    or 0 if target is not found. (So, if the first tested value in the list
    exactly matches target, 1 is returned.)

The search runs with 2 changing parameters:
  - startidx, the list index of the left cap of the search
  - width, the distance to the right cap (number of elements - 1)

There are 2 fixed parameters:
  - target, what we're looking for
  - datastart, set to 0x408000L

The parameters are assigned to the following registers:
  - %r10: target
  - %r11: startidx
  - %r12: width
  - %r13: datastart
  (Note: I chose these in order to run x86 directly on my laptop - not through
   the simulator. Originally datastart was r09, but this caused segfaults for
   some unknown reason.)

Also, the number of calls to "binary_search", which is the function output, is
accumulated in %rax.

If there are an even number of elements to search through, the tested element
is the one right BEFORE the exact midpoint.
  Examples: left Test right
    - [l l T r r]
    - [l l l T r r r r]

*)

let mbli_jampa_binary_search_test wid trg =
  [ text "binary_search"
      [ Incq,  [~%Rax]
      (* store checkidx in %r08 *)
      ; Movq,  [~%R12; ~%R08]
      ; Sarq,  [~$1; ~%R08] (* get w/2 *)
      ; Addq,  [~%R11; ~%R08]
      (* get checkval address, store in %r15 *)
      ; Movq,  [~$8; ~%R14]
      ; Imulq, [~%R08; ~%R14] (* R14 now contains byte offset
                                 from datastart, %r13 *)
      ; Movq,  [~%R13; ~%R15]
      ; Addq,  [~%R14; ~%R15]
      (* If width is 1 or 0, immediately end the recursion. *)
      ; Cmpq,  [~$1; ~%R12]
      ; J Eq,  [~$$"exit_wid_1"]
      ; Cmpq,  [~$0; ~%R12]
      ; J Eq,  [~$$"exit_wid_0"]
      (* store checkval in %r09 *)
      ; Movq,  [Ind2 R15; ~%R09]
      
      (* If checkval matches target, exit *)
      ; Cmpq,  [~%R10; ~%R09]
      ; J Eq,  [~$$"exit"]
      (* If checkval < target, we want to keep looking to the
         RIGHT. *)
      ; J Lt,  [~$$"RIGHT"]
      (* At this point, the only possibility is we look LEFT.
         startidx does not change.
         The new wid is (w-2)/2*)
      ; Subq,  [~$2; ~%R12]
      ; Sarq,  [~$1; ~%R12]
      ; Jmp,   [~$$"REC_CALL"]
      ]
  (* code to execute if checkval < target:
     startidx is checkidx+1.
     The new wid is (w-1)/2 *)    
  ; text "RIGHT"
      [ Movq,  [~%R08; ~%R11]
      ; Addq,  [~$1; ~%R11]
      ; Subq,  [~$1; ~%R12]
      ; Sarq,  [~$1; ~%R12]
      ]
  (* A recursive call to binary_search. *)
  ; text "REC_CALL"
      [ Callq, [~$$"binary_search"]
      ]
  (* Unconditional end to a call. *)
  ; text "exit"
      [ Retq,  []
      ]
  (* Set %rax to 0, then retq *)
  ; text "exit_search_fail"
      [ Movq,  [~$0; ~%Rax]
      ; Retq,  []
      ]
  (* Check if the current value is right. If so, retq.
     Otherwise, put -1 into %rax before returning.
     Remember that %r15 contains checkval_addr. *)
  ; text "exit_wid_0"
      [ Movq,  [Ind2 R15; ~%R08] (*******ERROR*******)
      ; Cmpq,  [~%R10; ~%R08]
      (* If no match, jump to search_fail. Otherwise, %rax
         holds the correct number of steps and we return. *)
      ; J Neq, [~$$"exit_search_fail"]
      ; Retq,  []
      ]
  (* Check if either the current value or the one right after
     is right. If so, retq. Otherwise, put -1 into %rax before
     returning.
     Remember that %r15 contains checkval_addr. *)
  ; text "exit_wid_1"
      [ Movq,  [Ind2 R15; ~%R08]
      ; Addq,  [~$8; ~%R15]
      ; Movq,  [Ind2 R15; ~%R09]
      (* Compare target (%r10) against both %r08 and %r09,
         the 2 values the binary search has been narrowed
         down to.
         If either one matches, jump to exit. Otherwise, jump to
         search_fail. *)
      ; Cmpq,  [~%R10; ~%R08]
      ; J Eq,  [~$$"exit"]
      ; Cmpq,  [~%R10; ~%R09]
      ; J Eq,  [~$$"exit"]
      ; Jmp,   [~$$"exit_search_fail"]
      ]
  
  ; gtext "main"
      (* Setup: populate sorted list starting at (%r13) *)
      (* A total of 20 values are written *)
      [ Movq,  [Imm (Lit 0x408000L); ~%R13]
      ; Movq,  [~$10; Ind3 (Lit 0L, R13)]
      ; Movq,  [~$12; Ind3 (Lit 8L, R13)]
      ; Movq,  [~$16; Ind3 (Lit 16L, R13)]
      ; Movq,  [~$50; Ind3 (Lit 24L, R13)]
      ; Movq,  [~$100; Ind3 (Lit 32L, R13)]
      
      ; Movq,  [~$250; Ind3 (Lit 40L, R13)]
      ; Movq,  [~$670; Ind3 (Lit 48L, R13)]
      ; Movq,  [~$1575; Ind3 (Lit 56L, R13)]
      ; Movq,  [~$12550; Ind3 (Lit 64L, R13)]
      ; Movq,  [~$123456; Ind3 (Lit 72L, R13)]
      
      ; Movq,  [~$900001; Ind3 (Lit 80L, R13)]
      ; Movq,  [~$1000000; Ind3 (Lit 88L, R13)]
      ; Movq,  [~$1000003; Ind3 (Lit 96L, R13)]
      ; Movq,  [~$1000007; Ind3 (Lit 104L, R13)]
      ; Movq,  [~$1000010; Ind3 (Lit 112L, R13)]
      
      ; Movq,  [~$1000013; Ind3 (Lit 120L, R13)]
      ; Movq,  [~$1000017; Ind3 (Lit 128L, R13)]
      ; Movq,  [~$1000099; Ind3 (Lit 136L, R13)]
      ; Movq,  [~$1000500; Ind3 (Lit 144L, R13)]
      ; Movq,  [~$9999999; Ind3 (Lit 152L, R13)]
      (* Setup: Initialize the 4 persistent registers - their
         values stay the same over multiple calls to binary_search *)
      ; Movq,  [~$trg; ~%R10]
      ; Movq,  [~$0; ~%R11]
      ; Movq,  [~$wid; ~%R12]
      ; Movq,  [~$0; ~%Rax]
      (* Call *)
      ; Callq, [~$$"binary_search"]
      ; Retq,  []
      ]
  ] in

  [ ("width:  0 | target :      10 (nonZ)",
    program_test (mbli_jampa_binary_search_test 0 10) 1L);
    ("width:  0 | target :      11 (   Z)",
    program_test (mbli_jampa_binary_search_test 0 11) 0L);
    ("width:  1 | target :      10 (nonZ)",
    program_test (mbli_jampa_binary_search_test 1 10) 1L);
    ("width:  1 | target :      12 (nonZ)",
    program_test (mbli_jampa_binary_search_test 1 12) 1L);
    ("width:  1 | target :      11 (   Z)",
    program_test (mbli_jampa_binary_search_test 1 11) 0L);
    ("width:  2 | target :       5 (   Z)",
    program_test (mbli_jampa_binary_search_test 2 5) 0L);
    ("width:  2 | target :      10 (nonZ)",
    program_test (mbli_jampa_binary_search_test 2 10) 2L);
    ("width:  2 | target :      12 (nonZ)",
    program_test (mbli_jampa_binary_search_test 2 12) 1L);
    ("width:  2 | target :      16 (nonZ)",
    program_test (mbli_jampa_binary_search_test 2 16) 2L);
    (* check some values that aren't in the table - width 19 *)
    ("width: 19 | target :       1 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 1) 0L);
    ("width: 19 | target :      51 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 51) 0L);
    ("width: 19 | target :     671 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 671) 0L);
    ("width: 19 | target :   12551 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 12551) 0L);
    ("width: 19 | target : 1000001 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 1000001) 0L);
    ("width: 19 | target : 1000012 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 1000012) 0L);
    ("width: 19 | target : 1000123 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 1000123) 0L);
    ("width: 19 | target : 5000000 (   Z)",
    program_test (mbli_jampa_binary_search_test 19 5000000) 0L);
    (* check some values that aren't in the table - width 18 *)
    ("width: 18 | target :       1 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 1) 0L);
    ("width: 18 | target :      51 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 51) 0L);
    ("width: 18 | target :     671 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 671) 0L);
    ("width: 18 | target :   12551 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 12551) 0L);
    ("width: 18 | target : 1000001 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 1000001) 0L);
    ("width: 18 | target : 1000012 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 1000012) 0L);
    ("width: 18 | target : 1000123 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 1000123) 0L);
    ("width: 18 | target : 5000000 (   Z)",
    program_test (mbli_jampa_binary_search_test 18 5000000) 0L);
    (* check all values in table - width 19*)
    ("width: 19 | target :      10 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 10) 4L);
    ("width: 19 | target :      12 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 12) 3L);
    ("width: 19 | target :      16 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 16) 4L);
    ("width: 19 | target :      50 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 50) 4L);
    ("width: 19 | target :     100 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 100) 2L);
    ("width: 19 | target :     250 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 250) 4L);
    ("width: 19 | target :     670 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 670) 3L);
    ("width: 19 | target :    1575 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1575) 4L);
    ("width: 19 | target :   12550 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 12550) 4L);
    ("width: 19 | target :  123456 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 123456) 1L);
    ("width: 19 | target :  900001 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 900001) 4L);
    ("width: 19 | target : 1000000 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000000) 3L);
    ("width: 19 | target : 1000003 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000003) 4L);
    ("width: 19 | target : 1000007 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000007) 4L);
    ("width: 19 | target : 1000010 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000010) 2L);
    ("width: 19 | target : 1000013 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000013) 4L);
    ("width: 19 | target : 1000017 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000017) 4L);
    ("width: 19 | target : 1000099 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000099) 3L);
    ("width: 19 | target : 1000500 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 1000500) 4L);
    ("width: 19 | target : 9999999 (nonZ)",
    program_test (mbli_jampa_binary_search_test 19 9999999) 4L);
    (* check all values in table - width 18*)
    ("width: 18 | target :      10 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 10) 4L);
    ("width: 18 | target :      12 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 12) 3L);
    ("width: 18 | target :      16 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 16) 4L);
    ("width: 18 | target :      50 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 50) 4L);
    ("width: 18 | target :     100 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 100) 2L);
    ("width: 18 | target :     250 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 250) 4L);
    ("width: 18 | target :     670 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 670) 3L);
    ("width: 18 | target :    1575 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1575) 4L);
    ("width: 18 | target :   12550 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 12550) 4L);
    ("width: 18 | target :  123456 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 123456) 1L);
    ("width: 18 | target :  900001 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 900001) 4L);
    ("width: 18 | target : 1000000 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000000) 3L);
    ("width: 18 | target : 1000003 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000003) 4L);
    ("width: 18 | target : 1000007 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000007) 4L);
    ("width: 18 | target : 1000010 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000010) 2L);
    ("width: 18 | target : 1000013 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000013) 4L);
    ("width: 18 | target : 1000017 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000017) 3L);
    ("width: 18 | target : 1000099 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000099) 4L);
    ("width: 18 | target : 1000500 (nonZ)",
    program_test (mbli_jampa_binary_search_test 18 1000500) 4L);
  ]

let kainino_tests =
  let rec zeroes n =
    begin match n with
    | 0 -> []
    | n -> (Quad (Lit 0L)) :: zeroes (n-1)
    end
  in

  let sieve n =
    [ text "sieve"
      [ Movq,    [ ~$2; ~%R08 ]
      ; Movq,    [ ~%Rsi; ~%Rcx ]
      ]
    ; text "L_setup"
      [ Movq,   [ ~%R08; Ind2 Rcx ]
      ; Incq,   [ ~%R08 ]
      ; Addq,   [ ~$8; ~%Rcx ]
      ; Cmpq,   [ ~%R08; ~%Rdi ]
      ; J Neq,   [ ~$$"L_setup" ]
      ; Movq,   [ ~%Rcx; ~%Rbx ]
      ; Movq,   [ ~%Rsi; ~%Rcx ]
      ]
    ; text "L_sieve"
      [ Movq,    [ Ind2 Rcx; ~%Rax ]
      ; Cmpq,    [ ~$0; ~%Rax ]
      ; J Eq,    [ ~$$"L_sieve_inc" ]
      ; Movq,    [ ~%Rcx; ~%R10 ]
      ; Imulq,   [ ~$8;  ~%Rax ]
      ]
    ; text "L_sieve_iter"
      [ Addq, [ ~%Rax; ~%R10 ]
      ; Movq, [ ~$0; Ind2 R10  ]
      ; Cmpq, [ ~%Rbx; ~%R10 ]
      ; J Lt, [ ~$$"L_sieve_iter" ]
      ]
    ; text "L_sieve_inc"
      [ Addq, [ ~$8; ~%Rcx ]
      ; Cmpq, [ ~%Rcx; ~%Rbx ]
      ; J Neq, [ ~$$"L_sieve" ]
      ]
    ; text "L_search"
      [ Subq,  [ ~$8; ~%Rbx ]
      ; Movq,  [ Ind2 Rbx; ~%Rax ]
      ; Cmpq,  [ ~$0; ~%Rax ]
      ; J Neq, [ ~$$"exit" ]
      ; Cmpq,  [ ~%Rbx; ~%Rsi ]
      ; J Eq,  [ ~$$"exit" ]
      ; J Neq, [ ~$$"L_search" ]
      ]
    ; text "exit"
      [ Retq, []
      ]
    ; gtext "main"
      [ Movq,  [ ~$n; ~%Rdi ]
      ; Leaq,  [ Ind1 (Lbl "array"); ~%Rsi ]
      ; Addq,  [ ~$4; ~%Rsi ]
      ; Callq, [  ~$$"sieve" ]
      ; Retq,  []
      ]
    ; data "array" (zeroes n)
    ] in
    
    [
      ("sieve 10", program_test (sieve 10) 7L);
      ("sieve 16", program_test (sieve 16) 13L);
      ("sieve 156", program_test (sieve 156) 151L);
    ]

let lpena_tests =
  let quicksort arr =
    [ text "qsort"
           [ Movq,  [Ind3(Lit(16L),Rsp); ~%R09] (* Move LO into R09 *)
           ; Movq,  [Ind3(Lit(8L) ,Rsp); ~%R10] (* Move HI into R10 *)
           ; Cmpq,  [~%R09; ~%R10]              (* If LO >= HI, goto EXIT *)
           ; J Le,  [~$$"exit"]
           ]
    ; text "partition"
           [ Movq,  [Ind2 R09; ~%R11]           (* Swap arr[LO] and arr[HI] *)
           ; Movq,  [Ind2 R10; Ind2 R09]        (* and R11 is pivot value *)
           ; Movq,  [~%R11; Ind2 R10]
           ; Movq,  [~%R09; ~%R12]              (* R12 is looping index (i) *)
           ; Movq,  [~%R09; ~%R13]              (* R13 is index of store *)
           ]
    ; text "loop"
           [ Cmpq,  [Ind2 R12; ~%R11]           (* If arr[i] >= pivot, continue *)
           ; J Le,  [~$$"conditional"]
           ; Movq,  [Ind2 R12; ~%R14]           (* Swap arr[i] and store val *)
           ; Movq,  [Ind2 R13; Ind2 R12]
           ; Movq,  [~%R14; Ind2 R13]
           ; Addq,  [~$8; ~%R13]                (* Increment store index *)
           ]
    ; text "conditional"
           [ Addq,  [~$8; ~%R12]                (* i++ *)
           ; Cmpq,  [~%R12; ~%R10]              (* if i < HI then loop *)
           ; J Gt,  [~$$"loop"]
           ; Movq,  [Ind2 R13; ~%R14]           (* Swap store val and arr[HI] *)
           ; Movq,  [Ind2 R10; Ind2 R13]
           ; Movq,  [~%R14; Ind2 R10]
           ; Addq,  [~$8; ~%R13]
           ; Pushq, [~%R13]                     (* Push pivot index + 1 as LO *)
           ; Pushq, [~%R10]                     (* Push HI as HI *)
           ; Subq,  [~$16; ~%R13]
           ; Pushq, [~%R09]                     (* Push LO as LO *)
           ; Pushq, [~%R13]                     (* Push pivot index  - 1 as HI *)
           ; Callq, [~$$"qsort"]                (* Recursive calls *)
           ; Popq,  [~%R14]                     (* Pop arguments to qsort *)
           ; Popq,  [~%R14]
           ; Callq, [~$$"qsort"]
           ; Popq,  [~%R14]                     (* Pop arguments to qsort *)
           ; Popq,  [~%R14]
           ; Retq,  []                          (* Return *)
           ]
    ; gtext "main"
           [ Movq,  [~$$"array"; ~%R08]         (* Get index of the array *)
           ; Movq,  [~$8; ~%R09]                (* R09 is LO index *)
           ; Movq,  [Ind1(Lbl "array"); ~%R10]  (* R10 is HI index *)
           ; Imulq, [~$8; ~%R10]
           ; Addq,  [~%R08; ~%R09]
           ; Addq,  [~%R08; ~%R10]
           ; Pushq, [~%R09]                     (* Push LO *)
           ; Pushq, [~%R10]                     (* Push HI *)
           ; Callq, [~$$"qsort"]
           ; Popq,  [~%R14]                     (* Pop arguments to qsort *)
           ; Popq,  [~%R14]
           ]
    ; text "exit"
           [ Movq,  [~$$"array"; ~%Rax]         (* Rax gets index of the array *)
           ; Retq,  []
           ]
    ; data "array" @@
        (Quad (Lit (Int64.of_int @@ List.length arr))) ::
          (List.map (fun x -> Quad (Lit x)) arr)
    ]
  in

  let quicksort_test l () =
    let m = assemble (quicksort l) |> load in
    let arr =
      match run m |> map_addr with
      | None -> failwith "Rax has incorrect addr"
      | Some addr -> addr
    in
    let get_mem i =
      int64_of_sbytes @@ Array.to_list @@ Array.sub m.mem (arr + i*8) 8
    in
    let res = List.mapi (fun i _ -> get_mem (i+1)) l in
    let ans = List.sort Int64.compare l in
    if res <> ans
    then failwith (Printf.sprintf("quicksort test failed"))
    else ()
  in

  [ "1, 5, 3, 7", quicksort_test [1L; 5L; 3L; 4L]
  ; "6, 10, 11, -2, 5, 3, 2",
    quicksort_test [6L; 10L; 11L; -2L; 5L; 3L; 2L]
  ; "empty list", quicksort_test []
  ; "singleton",  quicksort_test [100L]
  ; "repeated elements", quicksort_test [5L; 2L; 3L; 2L; 7L]
  ]

let maxmcc_tests =
  let nil = 0xC15341 in
  let cnt = 50 in
  let start = -40 in
  let pen_ult = -50 in
  let ult = -1 in
  let heap_top = 0x400100 in

  (* Part 3 program: linked_list_max
   *
   * Description: Provides functionality to initialize and add elements to a
   * linked list. Given program adds cnt integers to the linked list, starting at
   * the value of start and incrementing by one for each subsequent element.
   * Additionally, two final elements, pen_ult and ult, are added manually to
   * allow for different test cases to be implemented. The program then finds
   * the maximum element of the list.
   *
   * Registers: *)
  
  let linked_list_max =
    [ text "main"
        [ Movq, [~$heap_top; ~%Rbp]
        ; Callq, [~$$"init"]
        ; Movq, [~$cnt; ~%Rcx]
        ; Movq, [~$start; ~%R08]
        ]
    ; text "lbl1"
        [ Cmpq, [~$1; ~%Rcx]
        ; J Lt, [~$$"lbl2"]
        ; Callq, [~$$"cons"]
        ; Incq, [~%R08]
        ; Decq, [~%Rcx]
        ; Jmp, [~$$"lbl1"]
        ]
    ; text "lbl2"
        [ Movq, [~$pen_ult; ~%R08]
        ; Callq, [~$$"cons"]
        ; Movq, [~$ult; ~%R08]
        ; Callq, [~$$"cons"]
        ; Callq, [~$$"max"]
        ; Retq, []
        ]
    ; text "init"
        [ Movq, [~$nil; Ind2 Rbp]
        ; Movq, [~%Rbp; ~%Rdx]
        ; Retq, []
        ]
    ; text "cons"
        [ Movq, [~%R08; Ind3 (Lit 8L, Rbp)]
        ; Movq, [~%Rdx; Ind3 (Lit 16L, Rbp)]
        ; Addq, [~$16; ~%Rbp]
        ; Movq, [~%Rbp; ~%Rdx]
        ; Retq, []
        ]
    ; text "max"
        [ Subq, [~$8; ~%Rsp]
        ; Cmpq, [Ind2 Rdx; ~$nil]
        ; J Eq, [~$$"exit"]
        ; Movq, [Ind3 (Lit (Int64.neg 8L), Rdx); Ind2 Rsp]
        ; Movq, [Ind2 Rdx; ~%Rdx]
        ; Callq, [~$$"max"]
        ]
    ; text "lbl3"
        [ Cmpq, [~%Rax; Ind2 Rsp]
        ; J Gt, [~$$"lbl4"]
        ; Addq, [~$8; ~%Rsp]
        ; Retq, []
        ]
    ; text "lbl4"
        [ Movq, [Ind2 Rsp; ~%Rax]
        ; Addq, [~$8; ~%Rsp]
        ; Retq, []
        ]
    ; text "exit"
        [ Movq, [Imm (Lit (Int64.min_int)); ~%Rax]
        ; Addq, [~$8; ~%Rsp]
        ; Retq, []
        ]
    ]
  in

  [ ("Linked_list_max", program_test linked_list_max 9L) ]

let nchodosh_tests =
  let gcd a b =
    [text "mod"
  	[ Cmpq, [~%R12; ~%R13]
  	; J Gt, [~$$"exit1"]
  	; Subq, [~%R13; ~%R12]
  	; Jmp, [~$$"mod"]
  	]
    ; text "exit1"
  	 [Retq, []]
    ; text "gcd"
  	 [ Cmpq,  [~$0; ~%R10]
           ; J Eq,  [~$$"exit"]
           ; Movq,  [~%R10; ~%R11]
           ; Movq,  [~%R10; ~%R13]
           ; Movq,  [~%Rax; ~%R12]
  	 ; Callq, [~$$"mod"]			      
           ; Movq,  [~%R12; ~%R10]
  	 ; Movq,  [~%R11; ~%Rax]
  	 ; Jmp,   [~$$"gcd"]
           ]
    ; text "exit"
           [ 
           Retq,  [] 
           ]
    ; gtext "main"
            [ Movq,  [~$a; ~%Rax]
  	  ; Movq,  [~$b; ~%R10]
  	  ; Callq, [~$$"gcd"]
            ; Retq,  []
            ]
    ]
  in

  [ ("gcd_8_12", program_test (gcd 8 12) 4L);
    ("gcd_12_8", program_test (gcd 12 8) 4L);
    ("gcd_prime", program_test (gcd 3533 4457) 1L);
    ("gcd_prime2", program_test (gcd 3533 (7*3533)) 3533L)	       	       
  ]

let palimar_tests =
  (* We implemented the gcd program.
     It can handle negative numbers, zeroes and positive numbers.
  	 It uses Euclid's algorithm for fast computation. *)

  let gcd a b =
    [ text "main"
      [ Movq,  [~$0; ~%Rax]
      ; Movq,  [~$a; ~%Rdi]
      ; Movq,  [~$b; ~%Rsi]
      ; Cmpq,  [~$0; ~%Rdi]
      ; J Lt,  [~$$"fix_Rdi"]
      ; Cmpq,  [~$0; ~%Rsi]
      ; J Lt,  [~$$"fix_Rsi"]
      ]
    ; text "loop"
      [ Cmpq,  [~$0; ~%Rdi]
      ; J Eq,  [~$$"exit"]
      ; Cmpq,  [~$0; ~%Rsi]
      ; J Eq,  [~$$"exit"]
      ; Cmpq,  [~%Rsi; ~%Rdi]
      ; J Eq,  [~$$"done"]
      ; J Lt,  [~$$"lesser"]
      ; J Gt,  [~$$"greater"]
      ]
    ; text "fix_Rdi"
      [ Negq, [~%Rdi] 
      ; Cmpq,  [~$0; ~%Rsi]
      ; J Lt,  [~$$"fix_Rsi"]
      ]
    ; text "fix_Rsi"
      [ Negq, [~%Rsi]
      ;	Jmp,   [~$$"loop"]
      ]				
    ; text "greater"
      [ Subq, [~%Rsi; ~%Rdi] 
      ;	Jmp,   [~$$"loop"]
      ]
    ; text "lesser"
      [ Subq, [~%Rdi; ~%Rsi]
      ;	Jmp,   [~$$"loop"]
      ]
    ; text "done"
      [ Movq,  [~%Rsi; ~%Rax]
      ; Jmp, [~$$"exit"]
      ]
    ; text "exit"
     [ Retq,  [] ]
    ]
  in 

  [ ("gcd huge nums", program_test (gcd (124556234) (4222556)) 2L) ]

let samrossi_tests =
  let fib n =
    [ text "fib"
        [ Decq,   [~%Rdi]        (* arg1-- *)
        ; Cmpq,   [~$0; ~%Rdi]   (* if arg1 <= 0 then finish *)
        ; J Eq,   [~$$"exit"]
        ; Movq,   [~%Rdx; ~%Rax] (* x = arg3 *)
        ; Addq,   [~%Rsi; ~%Rdx] (* arg3 += arg2 *)
        ; Movq,   [~%Rax; ~%Rsi] (* arg2 = x *)
        ; Callq,  [~$$"fib"]     (* recurse *)
        ]
      ; text "exit"
        [ Movq,   [~%Rdx; ~%Rax]
        ; Addq,   [~$8; ~%Rsp]
        ; Retq,   []
        ]
      ; gtext "main"
        [ Movq,  [~$n; ~%Rdi] (* arg1 = n *)
        ; Movq,  [~$0; ~%Rsi] (* arg2 = 0 *)
        ; Movq,  [~$1; ~%Rdx] (* arg3 = 1 *)
        ; Callq, [~$$"fib"]
        ; Retq,  []
        ]
    ]
  in
  
  [ ("fib5", program_test (fib 5) 5L)
  ; ("fib6", program_test (fib 6) 8L)
  ]

let tgarsys_tests =
  let fibonacci_iter n =
    [ text "fib"
        [ Cmpq, [~$1; ~%Rdi]
        ; J Le, [~$$"exit"]
        ; Decq, [~%Rdi]
        ; Movq, [~%Rax; ~%R10]
        ; Addq, [~%R09; ~%R10]
        ; Movq, [~%R09; ~%R08]
        ; Movq, [~%Rax; ~%R09]
        ; Movq, [~%R10; ~%Rax]
        ; Jmp, [~$$"fib"]
        ]
      ; text "exit"
          [Retq, [] ]
      ; gtext "main"
          [ Movq, [~$n; ~%Rdi]
          ; Movq, [~$1; ~%R08]
          ; Movq, [~$1; ~%R09]
          ; Movq, [~$1; ~%Rax]
          ; Cmpq, [~$1; ~$n]
          ; J Gt, [~$$"fib"]  
          ; Retq,  []
          ]
    ]
  in
  
  (* Contrived function that adds up all pairs of numbers up to [n] *)
  let pair_adder n =
    [ text "inner_loop"
        [ Cmpq, [~%R10; ~%R11]
        ; J Gt, [~$$"outer_loop_post"]
        ; Addq, [~%R10; ~%Rax]
        ; Addq, [~%R11; ~%Rax]
        ; Incq, [~%R11]
        ; Jmp,  [~$$"inner_loop"]
        ]
    ; text "outer_loop_post"
        [ Incq, [~%R10]
        ; Movq, [~$0; ~%R11]
        ; Jmp,  [~$$"outer_loop"]
        ]
    ; text "outer_loop"
        [ Cmpq, [~%R08; ~%R10]
        ; J Gt, [~$$"exit"]
        ; Jmp, [~$$"inner_loop"]
        ]
    ; text "exit"
        [Retq, [] ]
    ; gtext "main"
        [ Movq, [~$n; ~%R08] (* Loop limit *)
        ; Movq, [~$0; ~%R10] (* Outer_loop counter *)
        ; Movq, [~$0; ~%R11] (* Inner_loop counter *)
        ; Movq, [~$0; ~%Rax] (* Answer *)
        ; Jmp,  [~$$"outer_loop"]
        ]
    ]
  in

  [ ("add_pairs 0", program_test (pair_adder 0) 0L)
  ; ("add_pairs 1", program_test (pair_adder 1) 3L)
  ; ("add_pairs 2", program_test (pair_adder 2) 12L)
  ; ("add_pairs 3", program_test (pair_adder 3) 30L)
  ; ("add_pairs 4", program_test (pair_adder 4) 60L)
  ; ("add_pairs 5", program_test (pair_adder 5) 105L)
  ; ("add_pairs 6", program_test (pair_adder 6) 168L)
  ; ("fib 0", program_test (fibonacci_iter 0) 1L)
  ; ("fib 1", program_test (fibonacci_iter 1) 1L)
  ; ("fib 2", program_test (fibonacci_iter 2) 2L)
  ; ("fib 3", program_test (fibonacci_iter 3) 3L)
  ; ("fib 4", program_test (fibonacci_iter 4) 5L)
  ; ("fib 5", program_test (fibonacci_iter 5) 8L)
  ]

let wmcd_tests =
  let binary_gcd n m =
    [ text "gcd"
         [ Cmpq,  [~%R08; ~%R09]
         ; J Eq,  [~$$"ret_n"]
         ; Cmpq,  [~$0; ~%R08]
         ; J Eq,  [~$$"ret_m"]
         ; Cmpq,  [~$0; ~%R09]
         ; J Eq,  [~$$"ret_n"]
         ; Movq,  [~%R08; ~%R10]
         ; Notq,  [~%R10]
         ; Andq,  [~$1; ~%R10]
         ; Cmpq,  [~$1; ~%R10]
         ; J Eq,  [~$$"even_n"]
         ; Movq,  [~%R09; ~%R11]
         ; Notq,  [~%R11]
         ; Andq,  [~$1; ~%R11]
         ; Cmpq,  [~$1; ~%R11]
         ; J Eq,  [~$$"even_m"]
         ; Cmpq,  [~%R09; ~%R08]
         ; J Gt,  [~$$"reduce"]
         ; Movq,  [~%R09; ~%R11]
         ; Subq,  [~%R08; ~%R11]
         ; Shrq,  [~$1; ~%R11]
         ; Movq,  [~%R08; ~%R09]
         ; Movq,  [~%R11; ~%R08]
         ; Callq, [~$$"gcd"]
         ; Retq,  []
         ]
    ; text "even_n"     
         [ Movq,  [~%R09; ~%R11]
         ; Andq,  [~$1; ~%R11]
         ; Cmpq,  [~$0; ~%R11]
         ; J Eq,  [~$$"both_even"]
         ; Shrq,  [~$1; ~%R08]
         ; Callq, [~$$"gcd"]
         ; Retq,  []
         ]
    ; text "even_m"
         [ Shrq,  [~$1; ~%R09]
         ; Callq, [~$$"gcd"]
         ; Retq,  []
         ]
    ; text "both_even"
         [ Shrq,  [~$1; ~%R08]
         ; Shrq,  [~$1; ~%R09]
         ; Callq, [~$$"gcd"]
         ; Shlq,  [~$1; ~%Rax]
         ; Retq,  []
         ]
    ; text "reduce"
         [ Subq,  [~%R09; ~%R08]
         ; Shrq,  [~$1; ~%R08]
         ; Callq, [~$$"gcd"]
         ; Retq,  []
         ]
    ; text "ret_n"
         [ Movq,  [~%R08; ~%Rax]
         ; Retq,  []
         ]
    ; text "ret_m"
         [ Movq,  [~%R09; ~%Rax]
         ; Retq,  []
         ]
    ; text "main"
         [ Movq,  [~$n; ~%R08]
         ; Movq,  [~$m; ~%R09]
         ; Callq, [~$$"gcd"]
         ; Retq,  []
         ]
    ]
  in

  [ ("gcd_36_24", program_test (binary_gcd 36 24) 12L)
  ; ("gcd_0_2", program_test (binary_gcd 0 2) 2L)
  ; ("gcd_2_2", program_test (binary_gcd 2 2) 2L)
  ; ("gcd_2_0", program_test (binary_gcd 2 0) 2L)
  ; ("gcd_0_0", program_test (binary_gcd 0 0) 0L)
  ; ("gcd_10_15", program_test (binary_gcd 10 15) 5L)
  ; ("gcd_21_49", program_test (binary_gcd 21 49) 7L)
  ; ("gcd_9_12", program_test (binary_gcd 9 12) 3L)
  ; ("gcd_35_17", program_test (binary_gcd 35 17) 1L)
  ]

let tests = asgoel_tests
          @ cbarcen_tests
          @ charcobb_tests
          @ chinz_tests
          @ dmally_tests
          @ ellisl_tests
          @ honki_tests
          @ isibner_tests
          @ jampa_tests
          @ kainino_tests
          @ lpena_tests
          @ maxmcc_tests
          @ nchodosh_tests
          @ palimar_tests
          @ samrossi_tests
          @ tgarsys_tests
          @ wmcd_tests

let other_team_tests = GradedTest("OtherTeamTests (manual)", 10, tests)
