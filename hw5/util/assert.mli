(* CIS341 Assertion Testing and Grading Infrastructure *)
(* Author: Steve Zdancewic                             *)

(* Do NOT modify this file -- we will overwrite it     *)
(* with our own version when testing your code.        *)

exception Timeout

(* An assertion is just a unit->unit function that either *)
(* succeeds silently or throws an Failure exception.       *)
type assertion = (unit -> unit)

type 'a test = 
  | GradedTest of string * int * (string * 'a) list
  | Test of string * (string * 'a) list

type suite = (assertion test) list


(**************)
(* Assertions *)

val assert_eq : 'a -> 'a -> assertion 
val assert_eqf : (unit -> 'a) -> 'a -> assertion
val assert_eqfs : (unit -> string) -> string -> assertion
val assert_fail : assertion 

val timeout_assert : int -> assertion -> assertion
val timeout_test : int -> assertion test -> assertion test
val timeout_suite : int -> suite -> suite

(***************************)
(* Generating Test Results *)

type result = 
  | Pass 
  | Fail of string

type outcome = (result test) list

val run_assertion : assertion -> result
val run_test : assertion test -> result test
val run_suite : suite -> outcome

(***********************)
(* Reporting functions *)

val result_test_to_string : string -> result test -> string 

(* val get_results result test -> (string * int * int * int * float * int * int) *)
val outcome_to_string :outcome -> string 
