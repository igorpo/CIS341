(* LLVMlite: A simplified subset of the LLVM IR *)

type uid = string                       (* Local identifiers  *)
type gid = string                       (* Global identifiers *)
type tid = string                       (* Named types        *)
type lbl = string                       (* Labels             *)

(* LLVM IR types *)
type ty =
  | Void                            (* mix of unit/bottom from C *)
  | I1 | I8 | I64                   (* integer types             *)
  | Ptr of ty                       (* t*                        *)
  | Struct of ty list               (* { t1, t2, ... , tn }      *)
  | Array of int * ty               (* [ NNN x t ]               *)
  | Fun of fty                      (* t1, ..., tn -> tr         *)
  | Namedt of tid                   (* named type aliases        *)

(* Function type: argument types and return type *)
and fty = ty list * ty 

(* Syntactic Values *)
type operand = 
  | Null            (* null pointer        *)
  | Const of int64  (* integer constant    *)
  | Gid   of gid    (* A global identifier *)
  | Id    of uid    (* A local identifier  *)

(* Type-annotated operands *)

(* Binary operations *)
type bop = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor

(* Comparison Operators *)
type cnd = Eq | Ne | Slt | Sle | Sgt | Sge

(* Instructions *)
type insn =
  | Binop of bop * ty * operand * operand      (* bop ty %o1, %o2                    *)
  | Alloca of ty                               (* alloca ty                          *)
  | Load of ty * operand                       (* load ty %u                         *)
  | Store of ty * operand * operand            (* store ty %t, ty* %u                *)
  | Icmp of cnd * ty * operand * operand       (* icmp %s ty %s, %s                  *)
  | Call of ty * operand * (ty * operand) list (* fn(%1, %2, ...)                    *)
  | Bitcast of ty * operand * ty               (* bitcast ty1 %u to ty2              *)
  | Gep of ty * operand * operand list         (* getelementptr ty* %u, i64 %vi, ... *)

(* Block terminators *)
type terminator =
  | Ret of ty * operand option   (* ret i64 %s                     *)
  | Br of lbl                    (* br label %lbl                  *)
  | Cbr of operand * lbl * lbl   (* br i1 %s, label %l1, label %l2 *)

(* Basic blocks *)
type block = { insns: (uid * insn) list; terminator: terminator }

(* Control Flow Graph: a pair of an entry block and a set labeled blocks *)
type cfg = block * (lbl * block) list

(* Function declarations *)
type fdecl = { fty: fty; param: uid list; cfg: cfg }

(* Initializers for global data *)
type ginit = 
  | GNull                     (* null literal             *)
  | GGid of gid               (* reference another global *)
  | GInt of int64             (* global integer value     *)
  | GString of string         (* constant global string   *)
  | GArray of gdecl list      (* global array             *)
  | GStruct of gdecl list     (* global struct            *)

(* Global declaration *)
and gdecl = ty * ginit
      
(* LLVMlite programs *)
type prog = 
  { tdecls: (tid * ty) list      (* named types *)
  ; gdecls: (gid * gdecl) list   (* global data *)
  ; fdecls: (gid * fdecl) list   (* code        *)
  ; edecls: (gid * ty) list      (* external declarations *)
  }

(* serializing --------------------------------------------------------------- *)

let mapcat s f l = String.concat s @@ List.map f l
let prefix p f a = p ^ f a
let ( ^. ) s t = if s = "" || t = "" then "" else s ^ t
let pp = Printf.sprintf

let rec string_of_ty : ty -> string = function
  | Void         -> "void" 
  | I1           -> "i1" 
  | I8           -> "i8"
  | I64          -> "i64"
  | Ptr ty       -> pp "%s*" (string_of_ty ty)
  | Struct ts    -> pp "{ %s }" (mapcat ", " string_of_ty ts)
  | Array (n, t) -> pp "[%s x %s]" (string_of_int n) (string_of_ty t)
  | Fun (ts,t)   -> pp "%s (%s)" (string_of_ty t) (mapcat ", " string_of_ty ts)
  | Namedt s     -> pp "%%%s" s

let sot = string_of_ty

let dptr = function
  | Ptr t -> t
  | _ -> failwith "PP: expected pointer type"

let string_of_operand : operand -> string = function
  | Null    -> "null"
  | Const i -> Int64.to_string i
  | Gid g   -> "@" ^ g
  | Id u    -> "%" ^ u

let soo = string_of_operand

let soop (t,v:ty * operand) : string =
  pp "%s %s" (sot t) (soo v)

let string_of_bop : bop -> string = function
    | Add -> "add"  | Sub  -> "sub"  | Mul  -> "mul" 
    | Shl -> "shl"  | Lshr -> "lshr" | Ashr -> "ashr"
    | And -> "and"  | Or   -> "or"   | Xor  -> "xor"

let string_of_cnd : cnd -> string = function
    | Eq  -> "eq"  | Ne  -> "ne"  | Slt -> "slt" 
    | Sle -> "sle" | Sgt -> "sgt" | Sge -> "sge"

let string_of_gep_index : operand -> string = function
  | Const i -> "i32 " ^ Int64.to_string i
  | o       -> "i64 " ^ soo o

let string_of_insn : insn -> string = function
  | Binop (b, t, o1, o2) -> pp "%s %s %s, %s"
                               (string_of_bop b) (sot t) (soo o1) (soo o2)
  | Alloca t             -> pp "alloca %s" (sot t)
  | Load (t, o)          -> pp "load %s, %s %s" (sot (dptr t)) (sot t) (soo o)
  | Store (t, os, od)    -> pp "store %s %s, %s %s" 
                               (sot t) (soo os) (sot (Ptr t)) (soo od)
  | Icmp (c, t, o1, o2)  -> pp "icmp %s %s %s, %s" 
                               (string_of_cnd c) (sot t) (soo o1) (soo o2)
  | Call (t, o, oa)      -> pp "call %s %s(%s)"
                               (sot t) (soo o) (mapcat ", " soop oa)
  | Bitcast (t1, o, t2)  -> pp "bitcast %s %s to %s" (sot t1) (soo o) (sot t2)
  | Gep (t, o, oi)       -> pp "getelementptr %s, %s %s, %s" (sot (dptr t)) (sot t) (soo o) 
                               (mapcat ", " string_of_gep_index oi)

let string_of_named_insn (u,i:uid * insn) : string =
  match i with
  | Store _ | Call (Void, _, _) -> string_of_insn i
  | _ -> pp "%%%s = %s" u (string_of_insn i)

let string_of_terminator : terminator -> string = function
  | Ret (_, None)   -> "ret void"
  | Ret (t, Some o) -> pp "ret %s %s" (sot t) (soo o)
  | Br l            -> pp "br label %%%s" l
  | Cbr (o, l, m)   -> pp "br i1 %s, label %%%s, label %%%s" (soo o) l m

let string_of_block (b:block) : string =
  (mapcat "\n" (prefix "  " string_of_named_insn) b.insns ^. "\n")
  ^ (prefix "  " string_of_terminator) b.terminator

let string_of_cfg (e,bs:cfg) : string =
  let string_of_named_block (l,b) = l ^ ":\n" ^ string_of_block b in
  string_of_block e ^ "\n" ^. mapcat "\n" string_of_named_block bs

let string_of_named_fdecl (g,f:gid * fdecl) : string =
  let string_of_arg (t,u) = pp "%s %%%s" (sot t) u in
  let ts, t = f.fty in
  pp "define %s @%s(%s) {\n%s\n}\n" (sot t) g 
     (mapcat ", " string_of_arg List.(combine ts f.param))
     (string_of_cfg f.cfg)

let rec string_of_ginit : ginit -> string = function
  | GNull       -> "null"
  | GGid g      -> pp "@%s" g
  | GInt i      -> Int64.to_string i
  | GString s   -> pp "c\"%s\\00\"" s
  | GArray gis  -> pp "[ %s ]" (mapcat ", " string_of_gdecl gis)
  | GStruct gis -> pp "{ %s }" (mapcat ", " string_of_gdecl gis)

and string_of_gdecl (t,gi:gdecl) : string =
  pp "%s %s" (sot t) (string_of_ginit gi)

let string_of_named_gdecl (g,gd:gid * gdecl) : string =
  pp "@%s = global %s" g (string_of_gdecl gd)

let string_of_named_tdecl (n,t:tid * ty) : string =
  pp "%%%s = type %s" n (sot t)

let string_of_named_edecl (g,t:gid * ty) : string =
  match t with
  | Fun (ts, rt) -> pp "declare %s @%s(%s)" (string_of_ty rt) g 
                       (mapcat ", " string_of_ty ts)
  | _ -> pp "@%s = external global %s" g (string_of_ty t)

let string_of_prog (p:prog) : string =
    (mapcat "\n" string_of_named_tdecl p.tdecls ^. "\n\n")
  ^ (mapcat "\n" string_of_named_gdecl p.gdecls ^. "\n\n")
  ^ (mapcat "\n" string_of_named_fdecl p.fdecls ^. "\n\n")
  ^ (mapcat "\n" string_of_named_edecl p.edecls)

(* comparison for testing ----------------------------------------------------- *)

(* delete dummy uids before comparison *)
let compare_block (b:block) (c:block) : int =
  let del_dummy (u,i) =
    match i with
    | Store (_, _, _) -> "", i
    | Call (Void, _, _) -> "", i
    | _ -> u, i
  in
  Pervasives.compare
    {b with insns=List.map del_dummy b.insns}
    {c with insns=List.map del_dummy c.insns}



(* helper module for AST ------------------------------------------------------ *)

module IR = struct
    let define t gid args cfg =
      let ats, param = List.split args in
      gid, { fty=ats,t; param; cfg}

    (* ignore first label *)
    let cfg (lbs:(lbl * block) list) : cfg =
      match lbs with
      | [] -> failwith "cfg: no blocks!"
      | (_,b)::lbs -> b, lbs

    let entry insns terminator : (lbl * block) = "", { insns; terminator }
    let label lbl insns terminator = lbl, { insns; terminator }

    (* terminators *)
    let ret_void = Ret (Void, None)
    let ret t op = Ret (t, Some op)
    let br l = Br l
    let cbr op l1 l2 = Cbr (op, l1, l2)
end

module Parsing = struct

let gensym, reset =
  let c = ref 0 in
    ( fun (s:string) -> incr c; Printf.sprintf "_%s__%d" s (!c) )
  , ( fun () -> c := 0 )

end
