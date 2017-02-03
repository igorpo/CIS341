open Assert
open X86
open Simulator
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()

let reverse a = 
      [ text "main"
          [ Movq,  [~$a; ~%R08] (* number *)
          ; Movq,  [~$0; ~%R09] (* accumulated *)
          ; Callq, [~$$"reverse"]
          ; Retq,  []
          ]
      ; text "reverse"
          [ Cmpq,  [~$0; ~%R08] 
          ; J Eq,  [~$$"return"]
          ; Movq,  [~$0; ~%R11] (* R11 is counter *)
          ; Movq,  [~%R08; ~%R12]
          ; Callq, [~$$"divide_by_10"] 
          ; Movq,  [~%R08; ~%R10]
          ; Callq, [~$$"mod_10"] 
          ; Imulq, [~$10; ~%R09] (* multiplies acc by 10 *)
          ; Addq,  [~%R10; ~%R09]
          ; Movq,  [~%R12; ~%R08]
          ; Jmp,   [~$$"reverse"]
          ]
      ; text "divide_by_10"     (* divides R08 by 10, puts it R11*)
       	  [ Cmpq,  [~$10; ~%R12]
          ; J Lt,  [~$$"div_ret"]
          ; Subq,  [~$10; ~%R12]
          ; Addq,  [~$1; ~%R11]
          ; Jmp,   [~$$"divide_by_10"]
          ]
      ; text "mod_10"            (* stores R08 % 10 in R10 *)
       	  [ Cmpq,  [~$10; ~%R10]
          ; J Lt,  [~$$"mod_ret"]
          ; Subq,  [~$10; ~%R10]
          ; Jmp,   [~$$"mod_10"]
          ]
      ; text "div_ret"
      	  [ Movq,  [~%R11; ~%R12]
      	  ; Retq,  []
      	  ]
      ; text "mod_ret"
      	  [ Retq,  [] ]
      ; text "return"
       	  [ Movq,  [~%R09; ~%Rax]
          ; Retq,  []
          ]
      ]

let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  	("12", program_test (reverse 12) 21L);
  	("12345", program_test (reverse 12345) 54321L);
  	("222", program_test (reverse 222) 222L);
  	("9", program_test (reverse 9) 9L);
  	("17471", program_test (reverse 17471) 17471L);
  	("bigger number", program_test 
  		(reverse 256367) 763652L);
  	(* should not work for negatives! *)
  	("keep negs the same", program_test (reverse (-19)) (-19L)); 
  ])

] 
