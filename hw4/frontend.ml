open Ll
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None -> 
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some terminator ->
              (gs, einsns, [], None, (l, {insns; terminator})::blks)
           end
        | T t  -> (gs, einsns, [], Some t, blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator" 
    | Some terminator -> 
       let insns = einsns @ insns in
       ({insns; terminator}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.fty * Ll.operand =
    match List.assoc id c with
    | Fun ft, g -> ft, g
    | _ -> failwith @@ id ^ " not bound to a function"

end


(* Helper function *)
let bool_to_int64 (b:bool) : int64 =
  if b then 1L else 0L 

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are 
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TVoid  -> Void
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)
  | Ast.TFun f -> Fun (cmp_fty f)

and cmp_fty (ts,r:Ast.fty) : Ll.fty =
  List.map cmp_ty ts, cmp_ty r

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]

let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Generate code to allocate an array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = cmp_ty @@ TRef (RArray TInt) in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]

(* 
  typing of result expression 
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
  | Uop of unop * exp node 
*)
let rec expr_type (exp: Ast.exp) : Ast.ty = 
  begin match exp with 
  | CNull _ -> TVoid
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr _ -> TRef (RString)
  | CArr (t, _) -> t 
  | NewArr (t, _) -> t
  | Id _ -> TRef (RString) (* string ??? *)
  | Index (e1, _) -> expr_type e1.elt (* e1[e2] e1 should be type? *)
  | Bop (b, _, _) ->
    begin match b with 
    | Add | Sub | Mul | IAnd | IOr | Shl | Shr | Sar -> TInt
    | _ -> TBool
    end
  | Uop (u, _) ->
    begin match u with
    | Neg | Bitnot -> TInt
    | _ -> TBool
    end
  | _ -> failwith "unmatched"
  end

let cmp_bop (b: Ast.binop) (op1: Ll.operand) (op2: Ll.operand) (t: Ll.ty) : Ll.insn =
  begin match b with 
  | Add -> Ll.Binop (Ll.Add, t, op1, op2)
  | Sub -> Ll.Binop (Ll.Sub, t, op1, op2)
  | Mul -> Ll.Binop (Ll.Mul, t, op1, op2)
  | Eq -> Ll.Icmp (Ll.Eq, t, op1, op2)
  | Neq -> Ll.Icmp (Ll.Ne, t, op1, op2)
  | Lt -> Ll.Icmp (Ll.Slt, t, op1, op2)
  | Lte -> Ll.Icmp (Ll.Sle, t, op1, op2)
  | Gt -> Ll.Icmp (Ll.Sgt, t, op1, op2)
  | Gte -> Ll.Icmp (Ll.Sge, t, op1, op2)
  | And -> Ll.Binop (Ll.And, t, op1, op2)
  | Or -> Ll.Binop (Ll.Or, t, op1, op2)
  | IAnd -> Ll.Binop (Ll.And, t, op1, op2) (* bitwise? Ll support? *)
  | IOr -> Ll.Binop (Ll.Or, t, op1, op2) (* bitwise? Ll support? *)
  | Shl -> Ll.Binop (Ll.Shl, t, op1, op2)
  | Shr -> Ll.Binop (Ll.Lshr, t, op1, op2)
  | Sar -> Ll.Binop (Ll.Ashr, t, op1, op2)
  end

(* Compile an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to bitcast the 
     resulting gid to (Ptr I8)

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

   - we found it useful to write a helper function 
     cmp_exp_as_ty : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream
     that compiles an expression and optionally inserts a bitcast to the
     desired Ll type. This is useful for dealing with OAT identifiers that
     correspond to gids that don't quite have the type you want

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
  | Uop of unop * exp node    
*)


type arg_list = (Ll.ty * Ll.operand) list

  
(* let cmp_exp_as_ty : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream =
  failwith "" *)

let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  begin match exp.elt with 
  | Bop (b, e1, e2) -> 
    let t = expr_type (Bop (b, e1, e2)) in 
    let id = gensym "" in
    let t1, op1, strm1 = cmp_exp c e1 in 
    let t2, op2, strm2 = cmp_exp c e2 in  
    (cmp_ty t, Ll.Id id, strm2 >@ strm1 >@ [I (id, cmp_bop b op1 op2 t1)])
  | CNull t -> (cmp_ty t, Ll.Null,[])
  | CBool b -> (Ll.I1, Ll.Const (bool_to_int64 b),[])
  | CInt i -> (Ll.I64, Ll.Const i,[])
  | CStr s -> failwith "CStr"
  | CArr (typ, exp_node_list) -> failwith "CArr"
  | NewArr (typ, exp_node) -> failwith "NewArr"
  | Id i -> failwith "Id"
  | Index (exp_node1, exp_node2) -> failwith "Index" 
  (* let lookup_function (id:Ast.id) (c:t) : Ll.fty * Ll.operand = *)
  | Call (i, exp_node_list) -> 
    let (ty_list, typ), ll_op = Ctxt.lookup_function i c in
    let _, ty_op_list, streams = List.fold_left2 zip_args_w_type (c,[], []) ty_list exp_node_list in
    let lbl = gensym i in
    (typ, ll_op, streams >@ [I (lbl, Ll.Call (typ, ll_op, ty_op_list))])
  | Uop (unop1, exp_node) -> failwith "Uop"
  end   

and zip_args_w_type (c:Ctxt.t * arg_list * stream) (t:Ll.ty) (a:Ast.exp node) : Ctxt.t * arg_list * stream =
  let ctxt, args, str = c in
  let arg_t, arg_o, arg_s = cmp_exp ctxt a in
  (ctxt, args @ [(arg_t, arg_o)], str >@ arg_s)

(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)
let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  begin match stmt.elt with
  | Ret e_opt -> 
    (* Printf.printf "Ret e_opt";   *)
    begin match e_opt with
    | Some s -> 
      let typ, opr, strm = cmp_exp c s in 
      if rt = typ then
        (c, strm >@ [T (Ll.Ret (typ, Some opr))])
      else 
        failwith "Incompatible return types"
    | None -> 
      if rt = Void then (c, [T (Ll.Ret (Void, None))])
      else failwith "Expected a void return type"
    end
  | Decl vd ->
    (* Printf.printf "Decl vd";   *)
    let id, expr = vd in 
    let typ, opr, strm = cmp_exp c expr in
    let new_c = Ctxt.add c id (typ, opr) in
    (new_c, strm >@ [I (id, Ll.Store (typ, opr, Ll.Id id))] >@ [I (id, Ll.Alloca typ)])
  | Assn (exp_node1, exp_node2) -> failwith "Assn"
  | SCall (i, exp_node_list) -> failwith "SCall"
  | If (e, b1, b2) -> 
    let t, op, c_strm = cmp_exp c e in
    let cmp_b1 = cmp_block c rt b1 in
    let cmp_b2 = cmp_block c rt b2 in 
    let l0 = gensym "begin_if" in 
    let l1 = gensym "then" in
    let l2 = gensym "else" in 
    let l3 = gensym "end_if" in 
    let strm = c_strm 
      >@ [T (Ll.Cbr (op, l1, l2))] 
      >@ [L (l1)]
      >@ cmp_b1
      >@ [T (Br (l3))]
      >@ [L (l2)]
      >@ [T (Br (l3))]
      >@ cmp_b2
      >@ [L (l3)]
    in 
    (c, strm)
  | For (vd_list, exp_node_option,
           stmt_node_option, block1) -> failwith "For"
  | While (exp_node, block1) -> failwith "While"
  end
  

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts


let cmp_global_ctxt_hlpr (c:Ctxt.t) (p:Ast.decl) : Ctxt.t =
  c

(* Populate a context with bindings for global variables and functions,
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs (The constructors starting with C). *)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left cmp_global_ctxt_hlpr c p


(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from 


OAT:
   type fdecl =
  { rtyp : ty
  ; name : id
  ; args : (ty * id) list
  ; body : block        
  }

LLVM:
  fdecl = { fty: fty; param: uid list; cfg: cfg }
  
  fty = ty list * ty 

cfg_of_stream(elt list) -> Ll.cfg * (Ll.gid * Ll.gdecl) list
LLVM_CODE = []
fty = ([args[0], ... , ...],  rtyp) 
uid_list = [args[1], ... , ...]
cfg, rest = cfg_of_stream (lift LLVM_CODE)
return {fty, uid_list, cfg} * rest
 *)

type uid_l = Ll.uid list
type ty_l = Ll.ty list

let cmp_fdecl_helper (a:Ctxt.t * uid_l * ty_l * stream) (d:ty * id) : Ctxt.t * uid_l * ty_l * stream =
  a


let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let args = f.elt.args in
  let rtyp = f.elt.rtyp in
  let _ = f.elt.name in
  let ll_body = cmp_block c (cmp_ty rtyp) f.elt.body in
  let _, uid_list, ty_list, ll_code = List.fold_left cmp_fdecl_helper (c, [], [], []) args in
  let cfg, rest = cfg_of_stream (ll_code >@ ll_body) in
  let fty = (ty_list, cmp_ty rtyp) in
  ({ fty=fty; param=uid_list; cfg=cfg }, rest)


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations

    gdecl = ty * ginit

type ty =
  | Void                            (* mix of unit/bottom from C *)
  | I1 | I8 | I64                   (* integer types             *)
  | Ptr of ty                       (* t*                        *)
  | Struct of ty list               (* { t1, t2, ... , tn }      *)
  | Array of int * ty               (* [ NNN x t ]               *)
  | Fun of fty                      (* t1, ..., tn -> tr         *)
  | Namedt of tid                   (* named type aliases        *)

    type ginit = 
  | GNull                     (* null literal             *)
  | GGid of gid               (* reference another global *)
  | GInt of int64             (* global integer value     *)
  | GString of string         (* constant global string   *)
  | GArray of gdecl list      (* global array             *)
  | GStruct of gdecl list     (* global struct            *)

 *)

let rec cmp_gexp (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  begin match e.elt with 
  | CNull t -> ((cmp_ty t, Ll.GNull),[])
  | CBool b -> ((Ll.I1, Ll.GInt(bool_to_int64 b)),[])
  | CInt i -> ((Ll.I64, Ll.GInt i),[])
  | CStr i -> ((Ll.Ptr Ll.I8, Ll.GString i),[])
  (* | CArr (t, e_list) -> ((cmp_ty t, Ll.GArray ), List.map cmp_gexp e_list) *)
  | _ -> failwith "other case"
  end


(* Oat initial context ------------------------------------------------------ *)
let internals =
  [ "oat_malloc",              Ll.Fun ([I64], Ptr I64)
  ; "oat_alloc_array",         Ll.Fun ([I64], Ptr (Struct [I64; Array (0, I64)]))
  ; "oat_assert_not_null",     Ll.Fun ([Ptr I8], Void)
  ; "oat_assert_array_length", Ll.Fun ([Ptr I64; I64], Void)
  ]

let builtins =
  [ "array_of_string",  cmp_ty @@ TFun ([TRef RString],  TRef(RArray TInt))
  ; "string_of_array",  cmp_ty @@ TFun ([TRef(RArray TInt)], TRef RString)
  ; "length_of_string", cmp_ty @@ TFun ([TRef RString],  TInt)
  ; "string_of_int",    cmp_ty @@ TFun ([TInt],  TRef RString)
  ; "string_cat",       cmp_ty @@ TFun ([TRef RString; TRef RString], TRef RString)
  ; "print_string",     cmp_ty @@ TFun ([TRef RString],  TVoid)
  ; "print_int",        cmp_ty @@ TFun ([TInt],  TVoid)
  ; "print_bool",       cmp_ty @@ TFun ([TBool], TVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> 
        Ctxt.add c i (t, Gid i)
      ) Ctxt.empty builtins in
  (* build global variable context *)
  let c = cmp_global_ctxt init_ctxt p in
  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
           let ll_gd, gs' = cmp_gexp gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.name,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in
  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls=[]; gdecls; fdecls; edecls }
