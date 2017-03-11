%{ open Ll
   open Ll.Parsing

%}

(* Symbols *)
%token STAR             (* * *)    
%token COMMA            (* , *)
%token COLON            (* : *)
%token EQUALS           (* = *)
%token LPAREN           (* ( *)
%token RPAREN           (* ) *)
%token LBRACE           (* { *)
%token RBRACE           (* } *)
%token LBRACKET         (* [ *)
%token RBRACKET         (* ] *)
%token EOF

(* Reserved Words *)
%token CROSS            (* x *)
%token I1               (* i1 *)
%token I8               (* i8 *)
%token I32              (* i32 *)
%token I64              (* i64 *)
%token TO               (* to *)
%token BR               (* br *)
%token EQ               (* eq *)
%token NE               (* ne *)
%token OR               (* or *)
%token AND              (* and *)
%token ADD              (* add *)
%token SUB              (* sub *)
%token MUL              (* mul *)
%token XOR              (* xor *)
%token SLT              (* slt *)
%token SLE              (* sle *)
%token SGT              (* sgt *)
%token SGE              (* sge *)
%token SHL              (* shl *)
%token RET              (* ret *)
%token TYPE             (* type *)
%token NULL             (* null *)
%token LSHR             (* lshr *)
%token ASHR             (* ashr *)
%token CALL             (* call *)
%token ICMP             (* icmp *)
%token VOID             (* void *)
%token LOAD             (* load *)
%token STORE            (* store *)
%token LABEL            (* label *)
%token ENTRY            (* entry *)
%token GLOBAL           (* global *)
%token DEFINE           (* define *)
%token DECLARE          (* define *)
%token EXTERNAL         (* external *)
%token ALLOCA           (* alloca *)
%token BITCAST          (* bitcast *)
%token GEP              (* getelementptr *)

%token <int> INT        (* int64 values *)
%token <string> LBL     (* labels *)
%token <string> GID     (* global identifier *)
%token <string> UID     (* local identifier *)
%token <string> STRING  (* string literals *)

%start <Ll.prog> prog
%%

prog:
  | ds=decls EOF
    { ds }

decls:
  | ds = decls_rev
    { { tdecls = List.rev ds.tdecls
      ; gdecls = List.rev ds.gdecls
      ; fdecls = List.rev ds.fdecls
      ; edecls = List.rev ds.edecls
    } }

decls_rev:
  | (* empty *)
    { { tdecls = [] ; gdecls = [] ; fdecls = [] ; edecls = [] } }
  | ds=decls_rev f=fdecl
    { { ds with fdecls = f :: ds.fdecls }  }
  | ds=decls_rev g=gdecl
    { { ds with gdecls = g :: ds.gdecls }  }
  | ds=decls_rev t=tdecl
    { { ds with tdecls = t :: ds.tdecls }  }
  | ds=decls_rev e=edecl
    { { ds with edecls = e :: ds.edecls }  }

fdecl:
  | DEFINE t=ty l=GID LPAREN a=arg_list RPAREN
    LBRACE eb=entry_block bs=block_list RBRACE
    { (l, { fty = (List.map fst a, t)
          ; param = List.map snd a
          ; cfg = (eb, bs)
          }
    ) }

gdecl:
  | g=GID EQUALS GLOBAL t=ty gi=ginit
    { (g, (t,gi)) }

tdecl:
  | tid=UID EQUALS TYPE t=ty
    { (tid, t) }

edecl:
  | DECLARE rt=ty g=GID LPAREN ts=separated_list(COMMA, ty) RPAREN
    { (g, Fun (ts,rt)) }
  | g=GID EQUALS EXTERNAL GLOBAL t=ty
    { (g, t) }

nonptr_ty: 
  | VOID { Void }
  | I1 { I1 }
  | I8 { I8 }    
  | I64 { I64 }
  | LBRACE ts=ty_list RBRACE
    { Struct ts }
  | LBRACKET i=INT CROSS t=ty RBRACKET
    { Array (i,t) }
  | rt=ty LPAREN ts=ty_list RPAREN
    { Fun (ts, rt) }
  | t=UID
    { Namedt t }

ty:
  | t=ty STAR
    { Ptr t }
  | t=nonptr_ty
    { t }

ty_list_rev:
  | t=ty
    { [t] }
  | ts=ty_list_rev COMMA t=ty
    { t::ts }

ty_list:
  | ts=ty_list_rev
    { List.rev ts }

arg:
  | t=ty u=UID { (t,u) }

arg_list_rev:
  | (* empty *)
    { [] }
  | a=arg
    { [a] }
  | args=arg_list_rev COMMA a=arg
    { a::args }

arg_list:
  | a=arg_list_rev
    { List.rev a }

operand:
  | NULL
    { Null }
  | i=INT
    { Const (Int64.of_int i) }
  | g=GID
    { Gid g }
  | u=UID
    { Id u }

ty_operand_list_rev:
  | (* empty *)
    { [] }
  | t=ty o=operand
    { [(t,o)] }
  | tos=ty_operand_list_rev COMMA t=ty o=operand
    { (t,o)::tos }

ty_operand_list:
  | tos=ty_operand_list_rev
    { List.rev tos }

i_operand_list_rev:
  | (* empty *)
    { [] }
  | I64 o=operand
      { [o] }
  | I32 o=operand
    { [o] }
  | os=i_operand_list_rev COMMA I64 o=operand
    { o::os }
  | os=i_operand_list_rev COMMA I32 o=operand
    { o::os }

i_operand_list:
  | os=i_operand_list_rev
    { List.rev os }

terminator:
  | RET t=ty o=operand
    { Ret (t, Some o) }
  | RET t=ty
    { Ret (t, None) }
  | BR LABEL l=UID
    { Br l }
  | BR I1 o=operand COMMA LABEL l1=UID COMMA LABEL l2=UID
    { Cbr (o,l1,l2) }

block:
  | is=insn_list t=terminator
    { { insns = is; terminator=t }  }

block_list_rev:
  | (* empty *)
    { [] }
  | bs=block_list_rev l=LBL COLON b=block
    { (l,b) :: bs }

block_list:
  | bs=block_list_rev
    { List.rev bs }

entry_block:
  | ENTRY COLON b=block
    { b }
  | b=block
    { b }

bop:
  | OR { Or }
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | SHL { Shl }
  | XOR { Xor }      
  | AND { And }
  | LSHR { Lshr }
  | ASHR { Ashr }

cnd:
  | EQ { Eq }
  | NE { Ne }
  | SLT { Slt }
  | SLE { Sle }
  | SGT { Sgt }
  | SGE { Sge }

insn:
  | u=UID EQUALS b=bop t=ty o1=operand COMMA o2=operand
    { (u, Binop (b,t,o1,o2)) }
  | u=UID EQUALS ALLOCA t=ty
    { (u, Alloca t) }
  | u=UID EQUALS LOAD ty COMMA t=ty o=operand
    { (u, Load (t,o)) }
  | STORE t1=ty o1=operand COMMA t2=ty o2=operand
    { (gensym "store", Store (t1,o1,o2)) }
  | u=UID EQUALS ICMP c=cnd t=ty o1=operand COMMA o2=operand
    { (u, Icmp (c,t,o1,o2)) }
  | CALL t=ty o=operand LPAREN args=ty_operand_list RPAREN
    { (gensym "call", Call (t, o, args)) }
  | u=UID EQUALS CALL t=ty o=operand LPAREN args=ty_operand_list RPAREN
    { (u, Call (t, o, args)) }
  | u=UID EQUALS BITCAST t1=ty o=operand TO t2=ty
    { (u, Bitcast (t1,o,t2)) }
  | u=UID EQUALS GEP ty COMMA t=ty o=operand COMMA os=i_operand_list
    { (u, Gep (t,o,os)) }

insn_list:
  | is=list(insn)
    { is }

gdecl_list_rev:
  | (* empty *)
    { [] }
  | t=ty g=ginit
    { [(t,g)] }
  | gs=gdecl_list_rev COMMA t=ty g=ginit
    { (t,g)::gs }

gdecl_list:
  | gs=gdecl_list_rev
    { List.rev gs }

ginit:
  | NULL
    { GNull }
  | g=GID
    { GGid g }
  | i=INT
    { GInt (Int64.of_int i) }
  | s=STRING
    { GString s }
  | LBRACKET gs=gdecl_list RBRACKET
    { GArray gs }
  | LBRACE gs=gdecl_list RBRACE
    { GStruct gs }
