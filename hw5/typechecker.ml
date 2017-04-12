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


let cflist_compare x y =
  if x.cfname < y.cfname then -1
  else if x.cfname > y.cfname then 1
  else 0

let flist_compare x y =
  if x.fname < y.fname then -1
  else if x.fname > y.fname then 1
  else 0

let compare_cflist_flist cf_list field_list =
  if (List.length cf_list) <> (List.length field_list) then false else
  List.for_all2 (fun x y -> x.cfname = y.fname) cf_list field_list

let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  begin match e.elt with
    | Ast.CNull ty -> ty
    | Ast.CBool bool_ -> Ast.TBool
    | Ast.CInt int64_ -> Ast.TInt
    | Ast.CStr string_ -> Ast.TRef (Ast.RString)
    | Ast.CArr (ty, exp_node_list) -> 
      if List.for_all (fun x -> (typecheck_exp c x) = ty) exp_node_list then (Ast.TRef (Ast.RArray ty))
      else type_error e "Array type mismatch"
    | Ast.CStruct (id, cfield_list) -> 
      let sorted_cf_list = List.sort cflist_compare cfield_list in
      let struct_opt = Tctxt.lookup_struct_option id c in
      begin match struct_opt with
      | Some field_list -> 
        let sorted_field_list = List.sort flist_compare field_list in
        if (compare_cflist_flist sorted_cf_list sorted_field_list) then Ast.TRef (Ast.RStruct id)
        else 
          let err_msg = "Struct " ^ id ^ " fields do not match" in
          type_error e err_msg
      | None -> let err_msg = "Unknown struct type " ^ id in
          type_error e err_msg
      end
    | Ast.Proj (exp_node, x) -> 
      let t = typecheck_exp c exp_node in
      begin match t with
      | Ast.TRef (Ast.RStruct (st_name)) -> 
          begin match Tctxt.lookup_field_option st_name x c with
          | Some s -> s
          | None -> let err_msg = x ^ " not a field in struct " ^ st_name in
          type_error e err_msg
          end
      | _ -> let err_msg = "Not a struct. Cannot project" in
          type_error e err_msg
      end
    | Ast.NewArr (ty, exp_node) -> 
      begin match typecheck_exp c exp_node with
      | Ast.TInt -> Ast.TRef (Ast.RArray ty)
      | _ -> let err_msg = "NewArr size expression must evaluate to an integer" in
          type_error e err_msg
      end
    | Ast.Id id -> 
      begin match Tctxt.lookup_option id c with
      | Some s -> s
      | None -> 
        begin match Tctxt.lookup_function_option id c with
        | Some (ty_list, ret_typ) -> 
          begin match ret_typ with
          | Ast.RetVal ty -> Ast.TRef (Ast.RFun (ty_list, Ast.RetVal ty)) 
          | Ast.RetVoid -> Ast.TRef (Ast.RFun (ty_list, Ast.RetVoid))
          end
        | None -> let err_msg = "Unknown identifier " ^ id in
          type_error e err_msg
        end
      end
    | Ast.Index (exp_node1, exp_node2) -> 
      let t1 = typecheck_exp c exp_node1 in
      let t2 = typecheck_exp c exp_node2 in
      begin match t1, t2 with
      | (Ast.TRef (Ast.RArray ty), Ast.TInt) -> ty
      | _ -> type_error e "Improper indexing. Try again."
      end
    | Ast.Call (exp_node, exp_node_list) -> 
      let t = typecheck_exp c exp_node in
      begin match t with
      | Ast.TRef (Ast.RFun (ty_list, Ast.RetVal ret_ty)) -> 
        if (List.length exp_node_list) <> (List.length ty_list) then 
          type_error e "Wrong number of arguments"
        else
          let flag = List.for_all2 (fun ex t1 -> t1 = (typecheck_exp c ex)) exp_node_list ty_list in
          if flag = true then ret_ty else 
          let msg = "argument types don't match" in type_error e msg
      | _ -> type_error e "Must be a non void function"
      end
    | Ast.Bop (binop_, exp_node1, exp_node2) -> 
      let (t1,t2,t) = typ_of_binop binop_ in
      let tc_t1 = typecheck_exp c exp_node1 in
      let tc_t2 = typecheck_exp c exp_node2 in
      if tc_t1 = t1 && tc_t2 = t2 then
        t
      else
        let msg = "Incompatible binop operand types" in
        type_error e msg
    | Ast.Uop (unop_, exp_node) -> 
      let (t1,t) = typ_of_unop unop_ in
      let tc_t1 = typecheck_exp c exp_node in
      if tc_t1 = t then
        t
      else
        type_error e "Incompatible unop operand types"
  end


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
     return statement, then the entire conditional statement might not return.

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)


let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * stmt_type =
  begin match s.elt with 
  | Ast.Assn (lhs, rhs) -> 
    let t1 = typecheck_exp tc lhs in
    let t2 = typecheck_exp tc rhs in
    if t1 <> t2 then
      type_error s "Assignment illegal, types do not match"
    else
    (tc, NoReturn)    
  | Ast.Decl vd -> 

    let id, exp_node = vd in
    begin match Tctxt.lookup_local_option id tc with
    | Some _ -> type_error s "Not allowed to redeclare variable"
    | None -> 
      let exp_ty = typecheck_exp tc exp_node in
      let new_c = Tctxt.add_local tc id exp_ty in
     (new_c, NoReturn)
    end

  | Ast.Ret e_opt -> 
    begin match e_opt with
    | Some ex -> 
      let typ_ex = typecheck_exp tc ex in
      begin match to_ret with
      | Ast.RetVal t1 -> 
        if t1 = typ_ex then (tc, Return) else type_error s "Return type mismatch"
      | _ -> type_error s "Expecting "
      end
    | None -> 
      begin match to_ret with
      | Ast.RetVal _ -> type_error s "Return type mismatch"
      | _ -> (tc, Return)
      end
    end
  | Ast.SCall (e, e_lst) -> 
    begin match typecheck_exp tc e with
    | Ast.TRef (Ast.RFun (ty_list, Ast.RetVoid)) -> 
      if (List.length e_lst) <> (List.length ty_list) then 
        type_error s "Wrong number of arguments"
      else
        let flag = List.for_all2 (fun ex t -> t = (typecheck_exp tc ex)) e_lst ty_list in
        if flag <> true then
          type_error s "Argument type mismatch"
        else
          (tc, NoReturn) 
    | _ -> type_error s "Tried to call non-void function"
    end
  | Ast.For (vd_lst, e_opt, stmt_opt, blk) -> 
    let (l2_ctxt, _) = 
      List.fold_left 
        (fun (ctxt,_) vd -> 
          typecheck_stmt ctxt (Ast.no_loc (Ast.Decl vd)) to_ret) (tc, NoReturn) vd_lst in
    let e_bool = begin match e_opt with
    | Some s1 -> typecheck_exp l2_ctxt s1
    | None -> Ast.TBool
    end in
    begin match e_bool with
    | Ast.TBool -> 
      let l3_ctxt = begin match stmt_opt with
      | Some s2 -> let c, _ = typecheck_stmt l2_ctxt s2 to_ret in c
      | None -> l2_ctxt
      end in
      let _ = typecheck_block l3_ctxt blk to_ret in
      (tc, NoReturn)
    | _ -> type_error s "Expression must be of type bool"
    end
  | Ast.While (e_n, blk) -> 
    begin match typecheck_exp tc e_n with
    | TBool ->
      let _ = typecheck_block tc blk to_ret in
      (tc, NoReturn)
    | _ -> type_error s "Expression must be of type bool"
    end
  | Ast.If (e_n, b1, b2) -> 
    begin match typecheck_exp tc e_n with
    | TBool ->
      let st_ty1 = typecheck_block tc b1 to_ret in
      let st_ty2 = typecheck_block tc b2 to_ret in
      begin match st_ty1, st_ty2 with
      | Return, Return -> (tc, Return)
      | _ -> (tc, NoReturn)
      end
    | _ -> type_error s "Expression must be of type bool"
    end
  end

and typecheck_block (tc : Tctxt.t) (body : Ast.block) (to_ret:ret_ty) : stmt_type =
  let _, f_st_ty = 
  List.fold_left (fun (ctxt, st_ty) stmt_n -> 
    let new_c, stmt_ty = typecheck_stmt ctxt stmt_n to_ret in
    begin match st_ty, stmt_ty with
    | Return, Return -> type_error (no_loc body) "Multiple return statements"
    | NoReturn, NoReturn -> (new_c, NoReturn)
    | _ -> (new_c, Return)  
    
    end
  ) (tc, NoReturn) body in (f_st_ty)


(* well-formed types -------------------------------------------------------- *)
(* TASK: Implement a (set of) functions that check that types are well formed.

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for type_error)

    - tc contains the structure definition context
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
    let typ_opt = Tctxt.lookup_struct_option id tc in
    begin match typ_opt with
      | Some field_list -> 
        ()(* List.iter (fun f -> typecheck_ty l tc f.ftyp) field_list *)
      | None -> type_error l "not in context"
    end
  | Ast.RArray ty -> typecheck_ty l tc ty
  | Ast.RFun fty -> typecheck_fty l tc fty
  end

and typecheck_ret_ty rt l tc =
  begin match rt with
  | Ast.RetVoid -> ()
  | Ast.RetVal ret_val_ty -> typecheck_ty l tc ret_val_ty
  end

and typecheck_fty (l : 'a Ast.node) (tc : Tctxt.t) (t:Ast.fty) : unit =
  let args_types, ret_typ = t in
  let _ = List.iter (fun arg -> typecheck_ty l tc arg) args_types in
  typecheck_ret_ty ret_typ l tc


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
  let rtyp = f.rtyp in
  let args = f.args in
  let body = f.body in
  let _ = typecheck_ret_ty rtyp l tc in
  let new_tc = List.fold_left (fun c (t, i) -> Tctxt.add_local c i t) tc args in
  let st_ty = typecheck_block new_tc body rtyp in 
  if st_ty <> Return then type_error l "Function must return"
  else
  ()

  (* 

    TODO: FINISH

   *)
  (* in () *)




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

let rec check_fdecl_redeclare c fname =
  begin match Tctxt.lookup_function_option fname c with 
  | Some _ -> true
  | None -> false
  end

let rec check_global_id_mention c gname =
  begin match Tctxt.lookup_global_option gname c with 
  | Some _ -> true
  | None -> false
  end

let create_struct_ctxt p =
  let c = Tctxt.empty in 
  List.fold_left (fun ctxt el ->
    match el with
    | Gtdecl ({elt=(id, fs)} as l) -> 
      begin match Tctxt.lookup_struct_option id ctxt with
      | Some s -> type_error l "Already declared struct with same name"
      | None -> 
        if check_dups fs then 
          type_error l "Duplicate fields in struct declaration"
        else 
          Tctxt.add_struct ctxt id fs 
      end
      
    | _ -> ctxt) c p


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let builtins_context = 
    List.fold_left (fun c (id, t) -> Tctxt.add_function c id t) tc builtins
  in
  List.fold_left (fun ctxt el ->
    match el with
    | Gfdecl ({elt=f} as l) -> 
      if check_fdecl_redeclare ctxt f.name then 
        type_error l "Trying to redeclare a function"
      else 
        let arg_types = List.map (fun (t,i) -> t) f.args in
        Tctxt.add_function ctxt f.name (arg_types,f.rtyp)
    | _ -> ctxt) builtins_context p

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun ctxt el ->
    match el with
    | Gvdecl gdec -> 
      let name = gdec.elt.name in
      let init = gdec.elt.init in
      if check_global_id_mention ctxt name then
        type_error gdec "Already declared global with same name"
      else
        begin match init.elt with
        | Id i -> 
            (if check_global_id_mention ctxt name then
              type_error gdec "Not allowed to reference global from global declaration"
            else
              begin match Tctxt.lookup_function_option i ctxt with
              | Some fun_det -> 
                Tctxt.add_global ctxt name (Ast.TRef (Ast.RFun fun_det))
              | None -> type_error gdec "Reference to nonexistent function name"
              end
              )
        | CNull t -> Tctxt.add_global ctxt name t
        | CBool _ -> Tctxt.add_global ctxt name Ast.TBool
        | CInt _ -> Tctxt.add_global ctxt name Ast.TInt
        | CStr _ -> Tctxt.add_global ctxt name (Ast.TRef (Ast.RString))
        | CArr (t, _) -> Tctxt.add_global ctxt name (Ast.TRef (Ast.RArray t))
        | CStruct (id, _) -> Tctxt.add_global ctxt name (Ast.TRef (Ast.RStruct id))
        | _ -> type_error gdec "Type not allowed in global declaration"
        end
    | _ -> ctxt) tc p

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
