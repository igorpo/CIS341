open Ast
open Tctxt

(* Use type_error to report error messages for ill-typed programs. *)
exception TypeError of string
let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* expressions -------------------------------------------------------------- *)
(* TASK:

   Typechecks an expression in the typing context c, returns the type of the 
   expression.  This function should implement the inference rules given in
   the oat.pdf specification.  There, they are written:

       F; S; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts:
        F - for function identifiers
        S - for structure definitions
        G - for global identifiers
        L - for local identifiers

   Notes:
     - Pay careful attention to the Id x case.  The abstract syntax treats
       function, global, and local identifiers all as Id x, but the 
       typechecking rules (and compilation invariants) treat function identifiers
       differently.

     - Structure values permit the programmer to write the fields in 
       any order (compared with the structure definition).  This means
       that, given the declaration 
          struct T { a:int; b:int; c:int } 
       The expression  
          new T {b=3; c=4; a=1}
       is well typed.  (You should sort the fields to compare them.)
       This is the meaning of the permutation pi that is used in the 
       TYP_STRUCTLIT rule.
*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
failwith "typecheck_exp not implemented"


(* statements --------------------------------------------------------------- *)

(* return behavior of a statement:
     - NoReturn:  might not return
     - Return: definitely returns 
*)
type stmt_type = NoReturn | Return

(* TASK: Typecheck a statement 
     - to_ret is the desired return type (from the function declaration
    
   This function should implement the statment typechecking rules from oat.pdf.  
   
   - In the TYP_IF rule, the "sup" operation is the least-upper-bound operation on the 
     lattice of stmt_type values given by the reflexive relation, plus:
           Return <: NoReturn
     Intuitively: if one of the two branches of a conditional does not contain a 
     return statement, then the entier conditional statement might not return.

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * stmt_type =
failwith "typecheck_stmt not implemented"


(* well-formed types -------------------------------------------------------- *)
(* TASK: Implement a (set of) functions that check that types are well formed.

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for type_error)

    - tc contains the structure definition context
*)

(* fs = [(x,bool), (y,bool)] *)


(* 

  l = ("Pair", [(x,bool), (y,bool)])

 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  begin match t with
  | Ast.TBool -> ()
  | Ast.TInt -> ()
  | TRef rty -> typecheck_ref l tc rty
  end

and typecheck_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  begin match t with
  | Ast.RString -> ()
  | Ast.RStruct id -> 
    let typ_opt = Tctxt.lookup_global_option id tc in
    begin match typ_opt with
      | Some typ -> typecheck_ty l tc typ
      | None -> raise (TypeError (id ^ " not in context"(*  ^ l.loc *)))
    end
  | Ast.RArray ty -> typecheck_ty l tc ty
  | Ast.RFun fty -> typecheck_fty l tc fty
  end

and typecheck_fty (l : 'a Ast.node) (tc : Tctxt.t) (t:Ast.fty) : unit =
  let args_types, ret_typ = t in
  let _ = List.iter (fun arg -> typecheck_ty l tc arg) args_types in
  begin match ret_typ with
  | Ast.RetVoid -> ()
  | Ast.RetVal ret_val_ty -> typecheck_ty l tc ret_val_ty
  end

and tc_bool (l : 'a Ast.node) (tc : Tctxt.t) : unit =
  let id, fs = l.elt in
  Printf.printf "ID is %s\n" id;
  ()
  (* begin match l.elt with
  | Ast.CBool b -> ()
  | Ast.Id id -> 
    begin match (Tctxt.lookup id tc) with
    | Ast.TBool -> ()
    | _ -> raise (TypeError (id ^ " is not bool"))
    end
  | _ -> failwith "not exp"
  end *)

(* and rec tc_int (l : 'a Ast.node) (tc : Tctxt.t) : unit =
  begin match l with
  
  end *)

(* 
  loc.elt = (id, fs)
  fs = [field]
 *)


let typecheck_tdecl (tc : Tctxt.t) l  (loc : 'a Ast.node) =
  List.iter (fun f -> typecheck_ty loc tc f.ftyp) l

(* function declarations ---------------------------------------------------- *)
(* TASK: typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node)  =
failwith "typecheck_fdecl unimplemented"


(* creating the typchecking context ----------------------------------------- *)

(* TASK: Complete the following functions that correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

   create_function_ctxt: - adds the the function identifiers and their
   types to the 'F' context (ensuring that there are no redeclared
   function identifiers)

   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> if List.exists (fun x -> x.fname = h.fname) t then true else check_dups t

let create_struct_ctxt p =
Tctxt.empty


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let builtins_context = 
    List.fold_left (fun c (id, t) -> Tctxt.add_function c id t) tc builtins
  in
(* failwith "create_function_ctxt undefined" *)
Tctxt.empty

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  Tctxt.empty

(* typechecks the whole program in the correct global context --------------- *)
(* This function implements the TYP_PROG rule of the oat.pdf specification.
   Note that global initializers are already checked in create_global_ctxt 
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc fs l 
    | _ -> ()) p
