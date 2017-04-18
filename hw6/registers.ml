;; open X86

(* Provides code to compute a histrogram of register usage for an x86 program.*)

type histogram = int array

let regs =
  [Rax ; Rbx ; Rcx ; Rdx ; Rsi ; Rdi ; Rbp ; Rsp
  ; R08 ; R09 ; R10 ; R11 ; R12 ; R13 ; R14 ; R15 ; Rip ]

let idx_of_reg = function
  | Rax -> 0 
  | Rbx -> 1 
  | Rcx -> 2 
  | Rdx -> 3 
  | Rsi -> 4 
  | Rdi -> 5 
  | Rbp -> 6 
  | Rsp -> 7 
  | R08 -> 8 
  | R09 -> 9 
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
  | Rip -> 16

let mk_empty () = Array.make 17 0

let r_incr (h:histogram) (r:reg) : unit =
  let idx = idx_of_reg r in
  h.(idx) <- h.(idx) + 1

let histogram_of_prog (p:prog) : histogram * int =
  let h = mk_empty () in
  let size = ref 0 in
  
  let h_opnd = function
    | Reg r | Ind2 r | Ind3 (_, r) -> r_incr h r
    | Imm _ | Ind1 _ -> ()
  in

  let h_ins (_, ops) = incr size; (List.iter h_opnd ops) in
  
  let h_elem {lbl; global; asm} =
    match asm with
    | Text insns -> List.iter h_ins insns
    | Data _ -> ()
  in
  let _ = List.iter h_elem p in
  h, !size

let string_of_histogram (h:histogram) : string =
  String.concat "\n" (List.mapi (fun i r -> Printf.sprintf "%s %5d " (string_of_reg r) h.(i)) regs)

(* Computes a "summary" of the histogram, which is just the count of registers used
   except that %rbp counts negatively, since it is used for stack slot access. *)
let summary (h:histogram) : int =
  (Array.fold_left (+) 0 h) - 2 * (h.(idx_of_reg Rbp))

