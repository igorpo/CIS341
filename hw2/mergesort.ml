open Assert
open Simulator
open X86
open Asm

let start_addr = 0x401F40L

let src = [-6790L;9300L;9462L;-1744L;-8049L;9684L;-3530L;-9883L;-9597L;-1897L
            ;-3785L;-156L;7515L;-8929L;-2993L;0L;4762L;-4876L;7554L;8709L;859L
            ;-9146L;-3786L;5873L;-7800L;-4122L;9760L;4305L;964L;-9927L;236L
            ;-967L;1962L;-7775L;4569L;0L;795L;-736L;4739L;-4074L;-8212L;-7811L
            ;-7125L;7555L;-5666L;4118L;-1814L]

let mergesort : prog =
  [ text "main"
      (* setup *)
      [ Movq, [Imm (Lit start_addr); ~%Rdx]
      ; Movq, [Imm (Lit (List.nth src 0)); Ind3 (Lit 0L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 1)); Ind3 (Lit 8L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 2)); Ind3 (Lit 16L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 3)); Ind3 (Lit 24L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 4)); Ind3 (Lit 32L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 5)); Ind3 (Lit 40L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 6)); Ind3 (Lit 48L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 7)); Ind3 (Lit 56L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 8)); Ind3 (Lit 64L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 9)); Ind3 (Lit 72L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 10)); Ind3 (Lit 80L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 11)); Ind3 (Lit 88L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 12)); Ind3 (Lit 96L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 13)); Ind3 (Lit 104L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 14)); Ind3 (Lit 112L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 15)); Ind3 (Lit 120L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 16)); Ind3 (Lit 128L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 17)); Ind3 (Lit 136L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 18)); Ind3 (Lit 144L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 19)); Ind3 (Lit 152L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 20)); Ind3 (Lit 160L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 21)); Ind3 (Lit 168L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 22)); Ind3 (Lit 176L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 23)); Ind3 (Lit 184L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 24)); Ind3 (Lit 192L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 25)); Ind3 (Lit 200L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 26)); Ind3 (Lit 208L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 27)); Ind3 (Lit 216L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 28)); Ind3 (Lit 224L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 29)); Ind3 (Lit 232L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 30)); Ind3 (Lit 240L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 31)); Ind3 (Lit 248L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 32)); Ind3 (Lit 256L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 33)); Ind3 (Lit 264L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 34)); Ind3 (Lit 272L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 35)); Ind3 (Lit 280L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 36)); Ind3 (Lit 288L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 37)); Ind3 (Lit 296L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 38)); Ind3 (Lit 304L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 39)); Ind3 (Lit 312L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 40)); Ind3 (Lit 320L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 41)); Ind3 (Lit 328L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 42)); Ind3 (Lit 336L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 43)); Ind3 (Lit 344L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 44)); Ind3 (Lit 352L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 45)); Ind3 (Lit 360L, Rdx)]
      ; Movq, [Imm (Lit (List.nth src 46)); Ind3 (Lit 368L, Rdx)]

      (* initialize value for merge sort *)
      ; Movq, [~%Rdx; ~%Rax]
      ; Movq, [~$47; ~%Rbx]
      ; Jmp, [~$$"merge_sort"]
      ]

  (* Merge Sort function
     Input: - Starting memory address in rax
            - Number of elements to be sorted in rbx
     Output: - Sorted array in the original memory locations *)
  ; text "merge_sort"
      (* If rbx <= 1 then no sorting needed, so return *)
      [ Cmpq, [~$1; ~%Rbx]
      ; J Le, [~$$"return"]

      (* Otherwise, partition the set by calculating the midpoint *)
      ; Movq, [~%Rbx; ~%Rcx]
      ; Sarq, [~$1; ~%Rcx]

      (* Sort the left part *)
      ; Pushq, [~%Rax]
      ; Pushq, [~%Rbx]
      ; Pushq, [~%Rcx]
      ; Movq, [~%Rcx; ~%Rbx] (* number of elements in first half *)
      ; Callq, [~$$"merge_sort"]
      ; Popq, [~%Rcx]
      ; Popq, [~%Rbx]
      ; Popq, [~%Rax]
      (* Sort the right part *)
      ; Pushq, [~%Rax]
      ; Pushq, [~%Rbx]
      ; Pushq, [~%Rcx]
      ; Subq, [~%Rcx; ~%Rbx] (* number of elements in the second half *)
      ; Shlq, [~$3; ~%Rcx]
      ; Addq, [~%Rcx; ~%Rax] (* starting position of the second half *)
      ; Callq, [~$$"merge_sort"]
      ; Popq, [~%Rcx]
      ; Popq, [~%Rbx]
      ; Popq, [~%Rax]
      (* merge the result *)
      (* allocate the stack for temporary space *)
      ; Movq, [~%Rbx; ~%Rdi]
      ; Shlq, [~$3; ~%Rdi]
      ; Subq, [~%Rdi; ~%Rsp]
      ; Movq, [~%Rsp; ~%Rdi]  (* dest addr *)
      (* for later use *)
      ; Pushq, [~%Rax]
      ; Pushq, [~%Rbx]
      (* set up pointer variables *)
      ; Pushq, [~%Rax]
      ; Subq, [~%Rcx; ~%Rbx] 
      ; Movq, [~%Rbx; ~%Rdx]
      ; Movq, [~%Rax; ~%Rbx]
      ; Movq, [~%Rcx; ~%Rax]
      ; Shlq, [~$3; ~%Rax]
      ; Addq, [~%Rax; ~%Rbx]
      ; Popq, [~%Rax]
      (* now rax=1st cur addr, rbx=2nd cur addr, rcx=1st cnt, rdx=2nd cnt *)
      ; Jmp, [~$$"start_merge"]
      ]
  ; text "start_merge"
      (* merge *)
      [ Cmpq, [~$0; ~%Rcx]
      ; J Eq, [~$$"check_2nd_half"]
      ; Cmpq, [~$0; ~%Rdx]
      (* 1st non-empty, 2nd empty *)
      ; J Eq, [~$$"pick_first"]
      (* both non-empty *)
      ; Jmp, [~$$"compare_elements"]
      ]
  ; text "check_2nd_half"
      (* merge *)
      [ Cmpq, [~$0; ~%Rdx]
      (* 1st empty, 2nd non-empty *)
      ; J Neq, [~$$"pick_second"]
      (* both non-empty *)
      ; Jmp, [~$$"return"]
      ]
  ; text "compare_elements"
      (* merge *)
      [ Pushq, [~%Rcx]
      ; Pushq, [~%Rdx]
      ; Movq, [Ind3 (Lit 0L, Rax); ~%Rcx]   (* head val of 1st half *)
      ; Movq, [Ind3 (Lit 0L, Rbx); ~%Rdx]   (* head val of 2nd half *)
      ; Cmpq, [~%Rdx; ~%Rcx] 
      ; Popq, [~%Rdx]
      ; Popq, [~%Rcx]
      ; J Le, [~$$"pick_first"]
      ; Jmp, [~$$"pick_second"]
      ]
  ; text "pick_first"
      [ Movq, [Ind3 (Lit 0L, Rax); ~%Rsi]  (* head val of 1st half *)
      ; Movq, [~%Rsi; Ind3 (Lit 0L, Rdi)]
      ; Addq, [~$8; ~%Rax]
      ; Addq, [~$8; ~%Rdi]
      ; Subq, [~$1; ~%Rcx]
      ; Jmp, [~$$"finish_merge"]
      ]
  ; text "pick_second"
      [ Movq, [Ind3 (Lit 0L, Rbx); ~%Rsi]  (* head val of 2nd half *)
      ; Movq, [~%Rsi; Ind3 (Lit 0L, Rdi)]
      ; Addq, [~$8; ~%Rbx]
      ; Addq, [~$8; ~%Rdi]
      ; Subq, [~$1; ~%Rdx]
      ; Jmp, [~$$"finish_merge"]
      ]
  ; text "finish_merge"
      [ Cmpq, [~$0; ~%Rcx]
      ; J Neq, [~$$"start_merge"]
      ; Cmpq, [~$0; ~%Rdx]
      ; J Neq, [~$$"start_merge"]
      (* copy the content of the stack back to memory *)
      ; Popq, [~%Rbx]
      ; Popq, [~%Rax]
      ; Pushq, [~%Rbx]
      ; Shlq, [~$3; ~%Rbx]
      ; Subq, [~%Rbx; ~%Rdi]  (* reset dest (now src) pointer *)
      ; Popq, [~%Rbx]
      ; Pushq, [~%Rbx] (* for later use *)
      ; Jmp, [~$$"start_copy"]
      ]
  ; text "start_copy"
      [ Movq, [Ind3 (Lit 0L, Rdi); ~%Rcx]
      ; Movq, [~%Rcx; Ind3 (Lit 0L, Rax)]
      ; Addq, [~$8; ~%Rax] 
      ; Addq, [~$8; ~%Rdi]
      ; Subq, [~$1; ~%Rbx]
      ; J Gt, [~$$"start_copy"]
      ; Popq, [~%Rbx]
      ; Shlq, [~$3; ~%Rbx]
      ; Addq, [~%Rbx; ~%Rsp]
      (* And.. we're done! *)
      ; Retq, [];
    ]
  ; text "return"
      [ Retq, [] ]
  ]

let get_sorted_list (m:mach) : int64 list =

  let idx = Int64.to_int @@ (Int64.sub start_addr mem_bot) in
  let len = List.length src in

  let get_one (m:mach) (i:int) : quad =
    int64_of_sbytes [ m.mem.(i); m.mem.(i+1); m.mem.(i+2); m.mem.(i+3)
                      ; m.mem.(i+4); m.mem.(i+5); m.mem.(i+6); m.mem.(i+7) ]
  in

  let rec get_all (res:int64 list) (offset:int) =
    if offset = len then res
	else (get_one m @@ idx+offset*8) :: (get_all res @@ offset+1)

  in get_all [] 0

let mergesort_test () =
  let m = assemble mergesort |> load in
  let _ = run m in

  if (get_sorted_list m) <> (List.sort Int64.compare src)
  then failwith (Printf.sprintf "merge sort failed")
  else ()
