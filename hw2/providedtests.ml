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
          [ Cmpq,  [~%R08; ~$0] 
          ; J Eq,  [~$$"return"]
          ; Callq, [~$$"divide_by_10"] 
          ; Callq, [~$$"mod_10"] 
          ; Imulq, [~$10; ~%R09] (* multiplies acc by 10 *)
          ; Addq,  [~%R10; ~%R09]
          ; Callq, [~$$"reverse"]
          ]
      ; text "divide_by_10"     (* divides R08 by 10, puts it R11*)
       	  [ Movq,  [~$0; ~%R11] (* R11 is counter *)
          ; Cmpq,  [~%R08; ~$10]
          ; J Lt,  [~$$"div_ret"]
          ; Subq,  [~$10; ~%R08]
          ; Addq,  [~$1; ~%R11]
          ]
      ; text "mod_10"            (* stores R08 % 10 in R10 *)
       	  [ Movq,  [~%R08; ~%R10]
       	  ; Cmpq,  [~%R10; ~$10]
          ; J Lt,  [~$$"mod_ret"]
          ; Subq,  [~%R10; ~$10]
          ; Jmp,   [~$$"mod_10"]
          ]
      ; text "div_ret"
      	  [ Movq,  [~%R11; ~%R08]
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
  	(* ("12", program_test (reverse 12L) 21L); *)
  ])

] 
