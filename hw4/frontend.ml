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
*)

let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
failwith "cmp_exp unimplemented"    

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
  failwith "cmp_stmt not implemented"

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts


(* Populate a context with bindings for global variables and functions,
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs (The constructors starting with C). *)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  failwith "cmp_global_ctxt unimplemented"


(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from 
 *)
let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  failwith "cmp_fdecl not implemented"


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations
 *)
let rec cmp_gexp (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  failwith "cmp_init not implemented"


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
