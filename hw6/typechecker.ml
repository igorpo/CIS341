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
(* Typechecks an expression in the typing context c, returns the type of the 
   expression.  This function should implement the inference rules given in
   the oad.pdf specification.  There, they are written:

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
  match e.elt with
  | CNull t -> t
  | CBool b -> TBool
  | CInt i -> TInt
  | CStr s -> TRef RString
  | CArr (a, l) ->
    let types_of = List.map (typecheck_exp c) l in
    if List.for_all ((=) a) types_of then TRef (RArray a)
    else type_error e "Mismatched array type"
  | NewArr (t, s) ->
      let size_type = typecheck_exp c s in
      if size_type = TInt then TRef (RArray t)
      else type_error s "Array size not an int"
  | Id i -> (match Tctxt.lookup_option i c with 
            | Some x -> x
            | None ->
              begin match Tctxt.lookup_function_option i c with
                | Some ft -> TRef (RFun ft)
                | None -> type_error e ("Unbound identifier " ^ i)
              end
            )
  | Bop (b, l, r) -> 
      let ltyp = typecheck_exp c l in
      let rtyp = typecheck_exp c r in
      let (bl, br, bres) = typ_of_binop b in
      if bl = ltyp then 
        if br = rtyp then bres
        else type_error r "Incorrect type in binary expression"       
      else type_error l "Incorrect type in binary expression"
  | Uop (u, e) ->
      let t = typecheck_exp c e in
      let (us, ures) = typ_of_unop u in
      if us = t then ures else type_error e "Incorrect type for unary operator"
  | Index (e1, e2) ->
      let arr_t = typecheck_exp c e1 in
      let ind_t = typecheck_exp c e2 in
      if ind_t = TInt then
        match arr_t with
        | TRef (RArray t) -> t
        | _ -> type_error e1 "Cannot index into non-array"
      else type_error e2 "Index of array index operator not an int"
  | Proj (s, id) ->
      let str_t = typecheck_exp c s in
      (match str_t with
      | TRef (RStruct sn) ->  
        (match Tctxt.lookup_field_option sn id c with
        | None -> type_error e (id ^ " not member of struct " ^ sn)
        | Some t -> t)
      | _ -> type_error s "Cannot project from non-struct")
  | CStruct (id, l) ->
      (match Tctxt.lookup_struct_option id c with
      | None -> type_error e (id ^ "not a struct type")
      | Some x ->
          let tc_field f = f.cfname, typecheck_exp c f.cfinit in
          let field_types = List.map tc_field l in
          let struct_names = List.sort compare (List.map (fun x -> x.fname) x) in
          let local_names = List.sort compare (List.map fst field_types) in
          if struct_names <> local_names 
          then type_error e "Mismatch of fields between struct definition and local declaration";
          List.iter (fun (id, ft) -> 
            let t = (List.find (fun i -> i.fname = id) x).ftyp in
            if t <> ft then type_error e (id ^ " field of struct incorrect")
            else ()) field_types;
          TRef (RStruct id))
  | Call (f, args) ->
      let argtyps = List.map (typecheck_exp c) args in
      match (typecheck_exp c f) with
      | TRef (RFun (l, RetVal r)) ->
          if List.length l <> List.length argtyps then type_error e "Incorrect number of arguments"
          else List.iter2 (fun arg l -> if arg <> l then type_error e "Incorrect type of argument") argtyps l;
          r
      | _ -> type_error e "Need function argument for function call"


(* statements --------------------------------------------------------------- *)

(* return behavior of a statement:
     - NoReturn:  might not return
     - Return: definitely returns 
*)
type stmt_type = NoReturn | Return

(* Typecheck a statement 
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
  match s.elt with
  | Assn (e1, e2) ->
    let assn_to = typecheck_exp tc e1 in
    let assn_from = typecheck_exp tc e2 in
    if assn_to = assn_from then tc, NoReturn else type_error s "Mismatched types in assignment"

  | Decl (id, exp) ->
      let exp_type = typecheck_exp tc exp in
      if List.exists (fun x -> fst x = id) tc.locals then type_error s "Cannot redeclare variable"
      else Tctxt.add_local tc id exp_type, NoReturn 

  | Ret r ->
      (match r, to_ret with
      | None, RetVoid -> tc, Return
      | Some r, RetVal to_ret -> 
          let t = typecheck_exp tc r in
          if t = to_ret then tc, Return
          else type_error s "Returned incorrect type" 
      | None, RetVal to_ret -> type_error s "Returned void in non-void function"
      | Some r, RetVoid -> type_error s "Returned non-void in void function")

  | SCall (f, args) -> 
      let argtyps = List.map (typecheck_exp tc) args in
      (match (typecheck_exp tc f) with
      | TRef (RFun (l, RetVoid)) ->
          if List.length l <> List.length argtyps then type_error s "Incorrect number of arguments"
          else List.iter2 (fun arg l -> if arg <> l then type_error s "Incorrect type of argument") argtyps l;
          tc, NoReturn
      | _ -> type_error s "Need function argument for function call")

  | If (e, b1, b2) ->
      let guard_type = typecheck_exp tc e in
      if guard_type <> TBool then type_error e "Incorrect type for guard"
      else
        let (_, lft_ret) = typecheck_block tc b1 to_ret in
        let (_, rgt_ret) = typecheck_block tc b2 to_ret in
        (match lft_ret, rgt_ret with
        | Return, Return -> tc, Return
        | _ -> tc, NoReturn)

  | While (b, bl) ->
      let guard_type = typecheck_exp tc b in
      if guard_type <> TBool then type_error b "Incorrect type for guard"
      else 
        let _ = typecheck_block tc bl to_ret in
        tc, NoReturn

  | For (vs, guard, s, b) ->
    let updated_context =
      List.fold_left (fun c (id, e) ->
        let t = typecheck_exp tc e in
        Tctxt.add_local c id t) tc vs in
    let _ = (match guard with
            | None -> ()
            | Some b -> if TBool <> typecheck_exp updated_context b then type_error b "Incorrect type for guard" else ()) in
    let _ = (match s with
            | None -> ()
            | Some s -> 
                let (nc, rt) = typecheck_stmt updated_context s to_ret in
                match rt with
                | NoReturn -> ()
                | Return   -> type_error s "Cannot return in for loop increment") in
    let _ = typecheck_block updated_context b to_ret in
    tc, NoReturn

and typecheck_block tc b to_ret =
  match b with
  | [] -> tc, NoReturn
  | [h] ->
      let c, r = typecheck_stmt tc h to_ret in tc, r
  | h1 :: h2 :: t ->
      let new_context, r = typecheck_stmt tc h1 to_ret in
      if r <> NoReturn then type_error h2 "Dead code"
      else typecheck_block new_context (h2 :: t) to_ret


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed.

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for type_error

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  begin match t with
    | TBool  -> ()
    | TInt   -> ()
    | TRef r -> typecheck_rt l tc r 
  end
  
and typecheck_ret_ty l tc (rt:Ast.ret_ty) : unit =
  begin match (rt:Ast.ret_ty) with
    | RetVoid -> ()
    | RetVal t -> typecheck_ty l tc t
  end

and typecheck_rt l tc (r:Ast.rty) : unit =
  begin match r with 
    | RString -> ()
    | RStruct id ->
      if Tctxt.lookup_struct_option id tc = None then type_error l "Unbound struct type" else ()
    | RArray t -> typecheck_ty l tc t
    | RFun (tl, rt) -> (typecheck_ret_ty l tc rt); List.iter (typecheck_ty l tc) tl
  end

let typecheck_tdecl (tc : Tctxt.t) l  (loc : 'a Ast.node) =
  List.iter (fun f -> typecheck_ty loc tc f.ftyp) l

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node)  =
  let updated = List.fold_left (fun c (t, i) -> Tctxt.add_local c i t) tc f.args in
  let _, returned = typecheck_block updated f.body f.rtyp in
  match returned with
  | NoReturn -> type_error l "Need return statement"
  | Return -> () 


(* creating the typchecking context ----------------------------------------- *)

(* The following functions that correspond to the
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
  | h :: t -> (List.exists (fun x -> x.fname = h.fname) t) || check_dups t


let create_struct_ctxt p =
  List.fold_left (fun c d ->
    match d with
    | Gtdecl ({elt=(id, fs)} as l) ->
        if check_dups fs then type_error l ("Repeated fields in " ^ id) 
        else if List.exists (fun (id',fs') -> check_dups (fs@fs')) c.structs
         then type_error l ("Duplicate fields in " ^ id)
        else if List.exists (fun x -> id = fst x) c.structs then
         type_error l ("Redeclaration of struct " ^ id)
        else Tctxt.add_struct c id fs
    | _ -> c) Tctxt.empty p


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let builtins_context = 
    List.fold_left (fun c (id, t) -> Tctxt.add_function c id t) tc builtins
  in
    List.fold_left (fun c d ->
      match d with
      | Gfdecl ({elt=f} as l)  ->
        if List.exists (fun x -> fst x = f.name) c.functions
        then type_error l ("Redeclaration of " ^ f.name)
        else Tctxt.add_function c f.name (List.map fst f.args, f.rtyp)
      | _ -> c) builtins_context p

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c d ->
    match d with
    | Gvdecl ({elt=decl} as l) ->  
        let e = typecheck_exp tc decl.init in
        if List.exists (fun x -> fst x = decl.name) c.globals
        then type_error l ("Redeclaration of " ^ decl.name)
        else Tctxt.add_global c decl.name e
    | _ -> c) tc p

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
