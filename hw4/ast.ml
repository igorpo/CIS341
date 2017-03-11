(* An AST node wraps decorates a datatype with its location in the source
   program. We attach source locations to expressions, statments, and
   top-level definitions to provide better error messages *)
type 'a node = { elt : 'a ; loc : Range.t }

let no_loc x = { elt=x; loc=Range.norange }

(* OAT identifiers *)
type id = string

(* OAT types *)
type ty =                               (* types of identifiers and exprs *)
  | TVoid
  | TBool 
  | TInt 
  | TRef of rty                         
  | TFun of fty

and rty =                               (* reference types *)
  | RString
  | RArray of ty

and fty = ty list * ty                  (* function types *)

(* Expressions *)
type unop =                             (* primitive operations *)
  | Neg | Lognot | Bitnot

type binop =                           
  | Add | Sub | Mul
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | IAnd | IOr
  | Shl | Shr | Sar

type exp =  
  | CNull of ty                         (* null literal for any TRef *)
  | CBool of bool                       (* bool literal *)
  | CInt of int64                       (* int literal *)
  | CStr of string                      (* string literal *)
  | CArr of ty * exp node list          (* array literal *)

  | NewArr of ty * exp node             (* zero-initialized arrays *)
  | Id of id                            (* identifiers *)
  | Index of exp node * exp node        (* index into an array *)
  | Call of id * exp node list          (* function call *)
  | Bop of binop * exp node * exp node  (* operations of two arguments *)
  | Uop of unop * exp node              (* operations with one argument *)

type vdecl = id * exp node              (* local variable declaration *)

(* statements *)
type stmt =
  | Assn of exp node * exp node         (* assignment *)
  | Decl of vdecl                       (* local variable declaration *)
  | Ret of exp node option              (* return a value or void *)
  | SCall of id * exp node list         (* call a void function *)
  | If of exp node * block * block      (* conditional *)
  | For of vdecl list * exp node option (* for loop *)
           * stmt node option * block  
  | While of exp node * block           (* while loop *)

(* blocks of statements *)
and block = stmt node list

(* global variable declarations *)
type gdecl =
  { name : id
  ; init : exp node
  } 

(* global function declarations *)
type fdecl =
  { rtyp : ty
  ; name : id
  ; args : (ty * id) list
  ; body : block        
  }

(* OAT programs *)
type decl =
  | Gvdecl of gdecl node
  | Gfdecl of fdecl node

type prog = decl list
