(* astlib.ml *)

(* Helper functions of abstract syntax of trees. *)
(******************************************************************************)

open Format
open Ast
open Range

(* Precedence for expressions and operators *)
(* Higher precedences bind more tightly     *)

let prec_of_binop = function
| Mul -> 100
| Add | Sub -> 90
| Shl | Shr | Sar -> 80
| Lt  | Lte | Gt | Gte -> 70
| Eq  | Neq -> 60
| And  -> 50
| Or   -> 40
| IAnd -> 30
| IOr  -> 20

let prec_of_unop = function _ -> 110

let prec_of_exp = function
| Bop (o,_,_) -> prec_of_binop o
| Uop (o,_) -> prec_of_unop o
| _ -> 130


(* Pretty Printer for AST *)
let string_of_unop = function
| Neg -> "-"
| Lognot -> "!"
| Bitnot -> "~"

let string_of_binop = function
| Mul   -> "*"
| Add   -> "+"
| Sub   -> "-"
| Shl   -> "<<"
| Shr   -> ">>"
| Sar   -> ">>>"
| Lt    -> "<"
| Lte   -> "<="
| Gt    -> ">"
| Gte   -> ">="
| Eq    -> "=="
| Neq   -> "!="
| And   -> "&"
| Or    -> "|"
| IAnd  -> "[&]"
| IOr   -> "[|]"

let print_id_aux fmt (x:id) = pp_print_string fmt x

let rec print_list_aux fmt sep pp l =
  begin match l with
    | []    -> ()
    | [h]   -> pp fmt h
    | h::tl -> pp fmt h; sep ();
	           print_list_aux fmt sep pp tl
  end

let rec print_ty_aux fmt t =
  let pps = pp_print_string fmt in
  match t with
  | TVoid       -> pps "void"
  | TBool       -> pps "bool"
  | TInt        -> pps "int"
  | TRef r      -> print_rty_aux fmt r
  | TFun (ts,r) -> print_ty_aux fmt r; pps "(";
                   print_list_aux fmt (fun () -> pps ", ") print_ty_aux ts;
                   pps ")"

and print_rty_aux fmt r =
  let pps = pp_print_string fmt in
  begin match r with
    | RString  -> pps "string"
    | RArray t -> print_ty_aux fmt t; pps "[]"
  end

and print_exp_aux level fmt e =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  let this_level = prec_of_exp e.elt in

  if this_level < level then pps "(";
  begin match e.elt with
    | CNull t -> print_ty_aux fmt t; pps "null"
    | CBool v -> pps (if v then "true" else "false")
    | CInt  v -> pps (Int64.to_string v)
    | CStr  v -> pps (Printf.sprintf "%S" v)
    | CArr (ty,vs) -> begin
        pps "new "; print_ty_aux fmt ty; pps "[]";
        pps "{";
        pp_open_hbox fmt ();
        print_list_aux fmt (fun () -> pps ","; pp_print_space fmt()) (print_exp_aux 0) vs;
        pp_close_box fmt ();
        pps "}";
      end
    | Id id -> print_id_aux fmt id
    | Index (e,i) -> print_exp_aux this_level fmt e; pps "["; print_exp_aux 0 fmt i; pps "]"
    | Call (id, es) -> print_id_aux fmt id; print_exps_aux "(" ")" fmt es
    | NewArr (ty, e1) ->
        pps "new "; print_ty_aux fmt ty;
        pps "["; print_exp_aux 0 fmt e1; pps "]"
    | Bop (o,l,r) ->
        pp_open_box fmt 0;
        print_exp_aux this_level fmt l;
        ppsp (); pps (string_of_binop o); ppsp ();
        print_exp_aux this_level fmt r;
        pp_close_box fmt ()
    | Uop (o,v) ->
        pp_open_box fmt 0;
        pps (string_of_unop o);
        print_exp_aux this_level fmt v;
        pp_close_box fmt ()
  end; if this_level < level then pps ")"

and print_exps_aux l r fmt es =
  let pps = pp_print_string fmt in
  pps l;
  pp_open_hvbox fmt 0;
  print_list_aux fmt
    (fun () -> pps ","; pp_print_space fmt())
    (fun fmt -> fun e -> print_exp_aux 0 fmt e) es;
  pp_close_box fmt ();
  pps r

let print_vdecl_aux semi fmt (id, init) =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  pp_open_hbox fmt ();
  pps "var "; print_id_aux fmt id;
  ppsp (); pps " ="; ppsp ();
  print_exp_aux 0 fmt init; pps semi;
  pp_close_box fmt ()

let rec print_block_aux fmt stmts =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  let ppnl = pp_force_newline fmt in

  if (List.length stmts) > 0 then
    begin pps "{"; ppnl (); pps "  ";
          pp_open_vbox fmt 0;
          print_list_aux fmt (fun () -> ppsp ()) print_stmt_aux stmts;
          pp_close_box fmt ();
          ppnl (); pps "}"
    end
  else pps "{ }"

and print_cond_aux fmt b_then opt_b_else =
  let pps = pp_print_string fmt in
  print_block_aux fmt b_then;
  begin match opt_b_else with
    | [] -> ()
    | b_else -> pps " else "; print_block_aux fmt b_else
  end

and print_stmt_aux fmt s =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in

  begin match s.elt with
    | Decl d -> print_vdecl_aux ";" fmt d

    | Assn (p,e) ->
        pp_open_box fmt 0;
	    print_exp_aux 0 fmt p;
	    pps " ="; ppsp ();
	    print_exp_aux 0 fmt e;
	    pps ";"; pp_close_box fmt ()

    | SCall (id, es) -> 
       print_id_aux fmt id; print_exps_aux "(" ")" fmt es; pps ";"

    | Ret (eo) ->
        pps "return";
        begin match eo with
          | None -> ()
          | Some e -> pps " "; print_exp_aux 0 fmt e
        end; pps ";"

    | If (e, b_then, opt_b_else) ->
        pps "if ("; print_exp_aux 0 fmt e; pps ") ";
        print_cond_aux fmt b_then opt_b_else

    | While(e, b) ->
	    pps "while ("; print_exp_aux 0 fmt e; pps ") ";
	    print_block_aux fmt b

    | For(decls, eo, so, body) ->
	    pps "for ("; pp_open_hvbox fmt 0;
        print_list_aux fmt (fun () -> pps ","; ppsp ()) (print_vdecl_aux "") decls;
        pps ";"; ppsp ();
	    begin match eo with
	      | None -> ();
	      | Some e -> print_exp_aux 0 fmt e;
	    end;
	    pps ";"; ppsp ();
	    begin match so with
	      | None -> ()
	      | Some s -> print_stmt_aux fmt s
	    end; pp_close_box fmt ();
	    pps ") "; print_block_aux fmt body
  end

let print_fdecl_aux fmt {elt={rtyp; name; args; body}} =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  let ppnl = pp_force_newline fmt in

  print_ty_aux fmt rtyp;
  pps @@ Printf.sprintf " %s(" name;
  pp_open_hbox fmt ();
  print_list_aux fmt (fun () -> pps ","; ppsp ())
    (fun fmt -> fun (t, id) ->
      print_ty_aux fmt t;
      pps " ";
      print_id_aux fmt id;
    ) args;
  pp_close_box fmt ();
  pps ") "; print_block_aux fmt body; ppnl ()

let print_gdecl_aux fmt (gd:Ast.gdecl) =
  let pps = pp_print_string fmt in
  let ppsp = pp_print_space fmt in
  pp_open_hbox fmt ();
  pps @@ Printf.sprintf "global %s =" gd.name; ppsp ();
  print_exp_aux 0 fmt gd.init; pps ";";
  pp_close_box fmt ()

let print_decl_aux fmt g =
  begin match g with
    | Gvdecl d -> print_gdecl_aux fmt d.elt
    | Gfdecl f -> print_fdecl_aux fmt f
  end

let print_prog_aux fmt p =
  let ppnl = pp_force_newline fmt in
  pp_open_vbox fmt 0;
  List.iter (fun g -> print_decl_aux fmt g; ppnl (); ppnl ()) p;
  pp_close_box fmt ()

let print ppx x : unit =
  pp_open_hvbox std_formatter 0;
  ppx std_formatter x;
  pp_close_box std_formatter ();
  pp_print_newline std_formatter ()

let string_of ppx x : string =
  pp_open_hvbox str_formatter 0;
  ppx str_formatter x;
  pp_close_box str_formatter ();
  flush_str_formatter ()

let print_prog (p:prog) : unit = print print_prog_aux p
let string_of_prog (p:prog) : string = string_of print_prog_aux p

let print_stmt (s:stmt node) : unit = print print_stmt_aux s
let string_of_stmt (s:stmt node) : string = string_of print_stmt_aux s

let print_block (b:block) : unit = print print_block_aux b
let string_of_block (b:block) : string = string_of print_block_aux b

let print_exp (e:exp node) : unit = print (print_exp_aux 0) e
let string_of_exp (e:exp node) : string = string_of (print_exp_aux 0) e

let print_ty (t:ty) : unit = print print_ty_aux t
let string_of_ty (t:ty) : string = string_of print_ty_aux t

(* AST to ML *)

let sp = Printf.sprintf

let ml_string_of_list (f: 'a -> string) (l: 'a list) : string =
  sp "[ %s ]" (String.concat " ; " (List.map f l))

let ml_string_of_option (f: 'a -> string) (o: 'a option) : string =
  begin match o with
  | None -> sp "None"
  | Some x -> sp "Some (%s)" (f x)
  end

(* TODO Change ml string printing for loc *)

let ml_string_of_node (f: 'a -> string) ({elt;loc}: 'a node) =
  sp "{ elt = %s; loc = %s }" (f elt) (Range.ml_string_of_range loc)

let rec ml_string_of_ty (t:ty) : string =
  match t with
  | TVoid -> "TVoid"
  | TBool -> "TBool"
  | TInt -> "TInt"
  | TRef r -> sp "TRef (%s)" (ml_string_of_reft r)
  | TFun (ts,r) -> sp "TFun (%s, %s)" 
                 (ml_string_of_list ml_string_of_ty ts)
                 (ml_string_of_ty r)

and ml_string_of_reft (r:rty) : string =
  match r with    
  | RString -> "RString"
  | RArray t -> sp "(RArray (%s))" (ml_string_of_ty t)


let ml_string_of_id : id -> string = (sp "\"%s\"")

let ml_string_of_binop : binop -> string = function
  | Add  -> "Add"
  | Sub  -> "Sub" 
  | Mul  -> "Mul" 
  | Eq   -> "Eq" 
  | Neq  -> "Neq" 
  | Lt   -> "Lt" 
  | Lte  -> "Lte" 
  | Gt   -> "Gt" 
  | Gte  -> "Gte" 
  | And  -> "And" 
  | Or   -> "Or" 
  | IAnd -> "IAnd" 
  | IOr  -> "IOr" 
  | Shl  -> "Shl" 
  | Shr  -> "Shr" 
  | Sar  -> "Sar" 

let ml_string_of_unop : unop -> string = function
  | Neg    -> "Neg"
  | Lognot -> "Lognot"
  | Bitnot -> "Bitnot"

let rec ml_string_of_exp_aux (e: exp) : string =
  begin match e with 
    | CNull t -> sp "CNull %s" (ml_string_of_ty t)
    | CBool b -> sp "CBool %b" b
    | CInt i -> sp "CInt %LiL" i
    | CStr s -> sp "CStr %S" s
    | CArr (t,cs) -> sp "CArr (%s,%s)" 
                         (ml_string_of_ty t) 
                         (ml_string_of_list ml_string_of_exp cs)
    | Id id -> sp "Id %s" (ml_string_of_id id)
    | Index (e, i) -> sp "Index (%s, %s)" 
                         (ml_string_of_exp e) (ml_string_of_exp i)
    | Call (id, exps) -> sp "Call (%s, %s)"
                            (ml_string_of_id id)
                            (ml_string_of_list ml_string_of_exp exps)
    | NewArr (t,e1) -> sp "NewArr (%s,%s)"
        (ml_string_of_ty t) (ml_string_of_exp e1)
    | Bop (b, e1, e2) -> sp "Bop (%s,%s,%s)"
        (ml_string_of_binop b) (ml_string_of_exp e1) (ml_string_of_exp e2)
    | Uop (u, e) -> sp "Uop (%s, %s)"
        (ml_string_of_unop u) (ml_string_of_exp e)
  end

and ml_string_of_exp (e:exp node) : string = 
  ml_string_of_node ml_string_of_exp_aux e

let ml_string_of_vdecl_aux (id,init:vdecl) : string =
  sp "(%s, %s)"
  (ml_string_of_id id) (ml_string_of_exp init)

let ml_string_of_vdecl (d:vdecl node) : string =
  ml_string_of_node ml_string_of_vdecl_aux d

let rec ml_string_of_stmt_aux (s:stmt) : string =
  match s with
  | Assn (p, e) -> sp "Assn (%s,%s)" (ml_string_of_exp p) (ml_string_of_exp e)
  | Decl d -> sp "Decl (%s)" (ml_string_of_vdecl_aux d)
  | Ret e -> sp "Ret (%s)" (ml_string_of_option ml_string_of_exp e)
  | SCall (id, exps) -> 
     sp "SCall (%s, %s)" (ml_string_of_id id) (ml_string_of_list ml_string_of_exp exps)
  | If (e,b1,b2) -> sp "If (%s,%s,%s)"
                       (ml_string_of_exp e) (ml_string_of_block b1) (ml_string_of_block b2)
  | For (d,e,s,b) -> sp "For (%s,%s,%s,%s)"
                        (ml_string_of_list ml_string_of_vdecl_aux d) 
                        (ml_string_of_option ml_string_of_exp e)
                        (ml_string_of_option ml_string_of_stmt s) (ml_string_of_block b)
  | While (e,b) -> sp "While (%s,%s)" (ml_string_of_exp e) (ml_string_of_block b)

and ml_string_of_stmt (s:stmt node) : string =
  ml_string_of_node ml_string_of_stmt_aux s

and ml_string_of_block (b:block) : string =
  ml_string_of_list ml_string_of_stmt b

let ml_string_of_args : (ty * id) list -> string =
  ml_string_of_list (fun (t,i) ->
    sp "(%s,%s)" (ml_string_of_ty t) (ml_string_of_id i))

let rec ml_string_of_fdecl_aux (f:fdecl) : string =
  sp "{ rtyp = %s; name = %s; args = %s; body = %s }"
  (ml_string_of_ty f.rtyp) (ml_string_of_id f.name)
  (ml_string_of_args f.args) (ml_string_of_block f.body)

and ml_string_of_fdecl (f:fdecl node) : string =
  ml_string_of_node ml_string_of_fdecl_aux f

let ml_string_of_gdecl_aux (gd:gdecl) : string =
  sp "{ name = %s; init = %s }"
     (ml_string_of_id gd.name) (ml_string_of_exp gd.init)

let ml_string_of_gdecl (d:gdecl node) : string =
  ml_string_of_node ml_string_of_gdecl_aux d

let ml_string_of_decl : decl -> string = function
  | Gvdecl d -> sp "Gvdecl (%s)" (ml_string_of_gdecl d)
  | Gfdecl f -> sp "Gfdecl (%s)" (ml_string_of_fdecl f)

let ml_string_of_prog : prog -> string =
  ml_string_of_list ml_string_of_decl

(* Checking AST equivalence *)
let eq_option (f: 'a -> 'a -> bool) (o1: 'a option) (o2: 'a option) : bool =
  begin match o1, o2 with
    | None, None -> true
    | Some a1, Some a2 -> f a1 a2
    | _ -> false
  end

let rec eq_list (f: 'a -> 'a -> bool) (l1: 'a list) (l2: 'a list) : bool =
  begin match l1, l2 with
    | [], [] -> true
    | h1::t1, h2::t2 -> f h1 h2 && eq_list f t1 t2
    | _ -> false
  end

let eq_node (f: 'a -> 'a -> bool) (l1: 'a node) (l2 : 'a node) : bool =
  f l1.elt l2.elt

let eq_id : id -> id -> bool = String.equal

let rec eq_ty (t1: ty) (t2: ty) : bool =
  match t1, t2 with
  | TVoid, TVoid 
  | TBool, TBool
  | TInt, TInt -> true
  | TRef r1, TRef r2 -> eq_rtyp r1 r2
  | TFun (ts1,r1), TFun (ts2,r2) -> eq_list eq_ty ts1 ts2 && eq_ty r1 r2
  | _ -> false

and eq_rtyp (r1:rty) (r2:rty) : bool =
  begin match r1, r2 with
    | RString, RString -> true
    | RArray t1, RArray t2 -> eq_ty t1 t2 
    | _ -> false
  end


let eq_unop : unop -> unop -> bool = (=)
let eq_binop : binop -> binop -> bool = (=)

let rec eq_exp_aux (e1:exp) (e2:exp) : bool =
  begin match e1, e2 with
    | CNull t1, CNull t2 -> eq_ty t1 t2
    | CBool b1, CBool b2 -> b1 = b2
    | CInt i1, CInt i2 -> i1 = i2
    | CStr s1, CStr s2 -> s1 = s2
    | CArr (t1,cs1), CArr (t2,cs2) -> eq_ty t1 t2 && eq_list eq_exp cs1 cs2
    | Id i1, Id i2 -> eq_id i1 i2
    | Index (e1,i1), Index (e2,i2) -> eq_exp e1 e2 && eq_exp i1 i2
    | Call (i1, es1), Call (i2, es2) -> eq_id i1 i2 && eq_list eq_exp es1 es2
    | NewArr (t1, e11), NewArr (t2, e21) ->
        eq_ty t1 t2 && eq_exp e11 e21
    | Bop (b1, e11, e12), Bop (b2, e21, e22) ->
        eq_binop b1 b2 && eq_exp e11 e21 && eq_exp e12 e22
    | Uop (u1, e1), Uop (u2, e2) ->
        eq_unop u1 u2 && eq_exp e1 e2
    | _ -> false
  end

and eq_exp (e1:exp node) (e2:exp node) : bool = eq_node eq_exp_aux e1 e2

let eq_vdecl_aux (i1,e1:vdecl) (i2,e2:vdecl) : bool =
  eq_id i1 i2 && eq_exp e1 e2

let eq_vdecl (d1:vdecl node) (d2:vdecl node) : bool =
  eq_node eq_vdecl_aux d1 d2


let rec eq_stmt_aux (s1: stmt) (s2: stmt) : bool =
  begin match s1, s2 with
  | Assn (p1, e1), Assn (p2, e2) -> eq_exp p1 p2 && eq_exp e1 e2
  | Decl d1, Decl d2 -> eq_vdecl_aux d1 d2
  | Ret eo1, Ret eo2 -> eq_option eq_exp eo1 eo2
  | SCall (x1, es1), SCall (x2, es2) -> eq_id x1 x2 && eq_list eq_exp es1 es2
  | If (e1, b11, b12), If (e2, b21, b22) ->
      eq_exp e1 e2 && eq_block b11 b21 && eq_block b12 b22
  | For (ds1, eo1, s1, b1), For (ds2, eo2, s2, b2) ->
      eq_list eq_vdecl_aux ds1 ds2 && eq_option eq_exp eo1 eo2 &&
      eq_option eq_stmt s1 s2 && eq_block b1 b2
  | While (e1, b1), While (e2, b2) -> eq_exp e1 e2 && eq_block b1 b2
  | _ -> false
  end

and eq_stmt (s1:stmt node) (s2:stmt node) : bool = eq_node eq_stmt_aux s1 s2
and eq_block (b1:block) (b2:block) : bool = eq_list eq_stmt b1 b2

let eq_args (a1:(ty * id) list) (a2:(ty * id) list) : bool =
  eq_list (fun (t1,i1) (t2, i2) -> eq_ty t1 t2 && eq_id i1 i2) a1 a2

let rec eq_fdecl_aux (f1:fdecl) (f2:fdecl) : bool =
  eq_ty f1.rtyp f2.rtyp && eq_id f1.name f2.name &&
  eq_args f1.args f2.args && eq_block f1.body f2.body

and eq_fdecl (f1:fdecl node) (f2:fdecl node) : bool = eq_node eq_fdecl_aux f1 f2

let eq_gdecl_aux (g1:gdecl) (g2:gdecl) : bool =
  eq_id g1.name g2.name && eq_exp g1.init g2.init

let eq_gdecl (d1:gdecl node) (d2:gdecl node) : bool =
  eq_node eq_gdecl_aux d1 d2

let eq_decl (g1:decl) (g2:decl) : bool =
  begin match g1, g2 with
  | Gvdecl d1, Gvdecl d2 -> eq_gdecl d1 d2
  | Gfdecl f1, Gfdecl f2 -> eq_fdecl f1 f2
  | _ -> false
  end

let eq_prog (p1: prog) (p2: prog) : bool = eq_list eq_decl p1 p2
