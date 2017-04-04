%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

/* Declare your tokens here. */
%token EOF
%token <int64>  INT
%token NULL
%token <string> STRING
%token <string> IDENT
%token <string> UIDENT

%token TINT     /* int */
%token TVOID    /* void */
%token TSTRING  /* string */
%token IF       /* if */
%token ELSE     /* else */
%token WHILE    /* while */
%token RETURN   /* return */
%token VAR      /* var */
%token STRUCT   /* struct */
%token SEMI     /* ; */
%token COMMA    /* , */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQEQ     /* == */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token GLOBAL   /* global */
%token FOR      /* for */
%token TBOOL    /* bool */
%token TRUE
%token FALSE
%token DOT      /* . */
%token NEW      /* new */
%token GT       /* > */
%token GTEQ     /* >= */
%token LT       /* < */
%token LTEQ     /* <= */
%token BANGEQ   /* != */
%token BAR      /* | */
%token AMPER    /* & */
%token IOR      /* [|] */
%token IAND     /* [&] */
%token LTLT     /* << */  
%token GTGT     /* >> */
%token GTGTGT   /* >>> */
%token ARROW    /* -> */
                       
%left IOR
%left IAND
%left BAR
%left AMPER
%left EQEQ BANGEQ
%left LT LTEQ GT GTEQ
%left LTLT GTGTGT GTGT 
%left PLUS DASH
%left STAR
%left DOT
%right ARROW
%nonassoc BANG
%nonassoc TILDE
%nonassoc LBRACKET
%nonassoc LPAREN

/* ---------------------------------------------------------------------- */

%start prog
%start exp_top
%start stmt_top
%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  | GLOBAL name=IDENT EQ init=gexp SEMI
    { Gvdecl (loc $startpos $endpos { name; init }) }
  | rtyp=ret_ty name=IDENT LPAREN args=arglist RPAREN body=block
    { Gfdecl (loc $startpos $endpos { rtyp; name; args; body }) }
  | STRUCT name=UIDENT LBRACE fs=separated_list(SEMI, decl_field) RBRACE 
    { Gtdecl (loc $startpos $endpos (name, fs)) }

decl_field:
  | t=ty id=IDENT { { fname=id; ftyp=t } }

arglist:
  | l=separated_list(COMMA, pair(ty,IDENT)) { l }
    
ty:
  | TINT   { TInt }
  | r=rtyp { TRef r }
  | LPAREN t=ty RPAREN { t } 
  | TBOOL  { TBool } 

%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline rtyp:
  | TSTRING { RString }
  | t=ty LBRACKET RBRACKET { RArray t }
  | id=UIDENT { RStruct id }
  | LPAREN RPAREN ARROW ret=ret_ty { RFun ([], ret) }
  | LPAREN t=ty RPAREN ARROW ret=ret_ty { RFun ([t], ret) }
  | LPAREN t=ty COMMA l=separated_list(COMMA, ty) RPAREN ARROW ret=ret_ty
       { RFun (t :: l, ret) }

%inline bop:
  | PLUS   { Add }
  | DASH   { Sub }
  | STAR   { Mul }
  | EQEQ   { Eq } 
  | BANGEQ { Neq }
  | LT     { Lt }
  | LTEQ   { Lte }
  | GT     { Gt }
  | GTEQ   { Gte }
  | AMPER  { And }
  | BAR    { Or }
  | IAND   { IAnd }
  | IOR    { IOr }
  | LTLT   { Shl }
  | GTGT   { Shr }
  | GTGTGT { Sar } 

%inline uop:
  | DASH  { Neg }
  | BANG  { Lognot }
  | TILDE { Bitnot }

gexp:
  | t=ty NULL  { loc $startpos $endpos @@ CNull t }
  | i=INT      { loc $startpos $endpos @@ CInt i } 
  | TRUE       { loc $startpos $endpos @@ CBool true }
  | FALSE      { loc $startpos $endpos @@ CBool false }
  | s=STRING   { loc $startpos $endpos @@ CStr s }
  | t=ty LBRACKET RBRACKET LBRACE cs=separated_list(COMMA, gexp) RBRACE
               { loc $startpos $endpos @@ CArr (t, cs) } 
  | i=UIDENT LBRACE fs=separated_list(SEMI, gfield) RBRACE
               { loc $startpos $endpos @@ CStruct (i, fs) }
  | id=IDENT {loc $startpos $endpos @@ Id id }

gfield:
  | id=IDENT EQ e=gexp { { cfname = id; cfinit = e } }

lhs:  
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
  | e=exp DOT id=IDENT  { loc $startpos $endpos @@ Proj (e, id) }

exp:
  | i=INT               { loc $startpos $endpos @@ CInt i }
  | t=ty NULL           { loc $startpos $endpos @@ CNull t }
  | e1=exp b=bop e2=exp { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp         { loc $startpos $endpos @@ Uop (u, e) }
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN
                        { loc $startpos $endpos @@ Call (e,es) }
  | LPAREN e=exp RPAREN { e } 
  | TRUE                { loc $startpos $endpos @@ CBool true }
  | FALSE               { loc $startpos $endpos @@ CBool false }
  | s=STRING            { loc $startpos $endpos @@ CStr s }
  | NEW t=ty LBRACKET RBRACKET LBRACE cs=separated_list(COMMA, exp) RBRACE
                        { loc $startpos $endpos @@ CArr (t, cs) }
  | NEW t=ty LBRACKET e1=exp RBRACKET
                        { loc $startpos $endpos @@ NewArr(t, e1) } 
  | e=exp DOT id=IDENT  { loc $startpos $endpos @@ Proj(e, id) }
  | NEW t=UIDENT LBRACE cs=separated_list(SEMI, field) RBRACE
                        { loc $startpos $endpos @@ CStruct(t, cs) }

field:
  | id=IDENT EQ e=exp { { cfname = id; cfinit = e } }

vdecl:
  | VAR id=IDENT EQ init=exp { (id, init) }

stmt: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN SEMI
                        { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt         { ifs }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp RPAREN b=block  
                        { loc $startpos $endpos @@ While(e, b) } 
  | FOR LPAREN ds=separated_list(COMMA, vdecl) SEMI e=exp? SEMI s=stmt? RPAREN b=block
                        { loc $startpos $endpos @@ For(ds,e,s,b) } 

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }
