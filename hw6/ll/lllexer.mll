{ open Lexing
  open Llparser

  exception SyntaxError of string
}

let newline = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = lowercase | uppercase
let digit = '-'? ['0'-'9']
let identifier = (character | digit | '_' ) (character | digit | '_' | '.')*

rule token = parse
  | eof                { EOF       }
  | whitespace+        { token lexbuf }
  | newline+           { token lexbuf }
  | "c\""              { read_string (Buffer.create 17) lexbuf }
  | '*'                { STAR      }
  | ','                { COMMA     }
  | ':'                { COLON     }
  | '='                { EQUALS    }
  | '('                { LPAREN    }
  | ')'                { RPAREN    }
  | '{'                { LBRACE    }
  | '}'                { RBRACE    }
  | '['                { LBRACKET  }
  | ']'                { RBRACKET  }
  | "i1"               { I1 }
  | "i8"               { I8 }
  | "i32"              { I32 }
  | "i64"              { I64 }
  | "to"               { TO }
  | "br"               { BR }
  | "eq"               { EQ }
  | "ne"               { NE }
  | "or"               { OR }
  | "and"              { AND }
  | "add"              { ADD }
  | "sub"              { SUB }
  | "mul"              { MUL }
  | "xor"              { XOR }
  | "slt"              { SLT }
  | "sle"              { SLE }
  | "sgt"              { SGT }
  | "sge"              { SGE }
  | "shl"              { SHL }
  | "ret"              { RET }
  | "getelementptr"    { GEP }
  | "type"             { TYPE }
  | "null"             { NULL }
  | "lshr"             { LSHR }
  | "ashr"             { ASHR }
  | "call"             { CALL }
  | "icmp"             { ICMP }
  | "void"             { VOID }
  | "load"             { LOAD }
  | "entry"            { ENTRY }
  | "store"            { STORE }
  | "label"            { LABEL }
  | "global"           { GLOBAL }
  | "define"           { DEFINE }
  | "declare"          { DECLARE }
  | "external"         { EXTERNAL }
  | "alloca"           { ALLOCA }
  | "bitcast"          { BITCAST }
  | '%' ('.' ?) (identifier as i) { UID i }
  | '@' ('.' ?) (identifier as i) { GID i }
  | "x"                           { CROSS } (* for Array types *)
  | digit+ as d                   { INT (int_of_string d) }
  | identifier as i               { LBL i }
  | ";" ([^ '\n' '\r'])* newline  { token lexbuf } (* comments *)
  | "declare" ([^ '\n' '\r'])* newline { token lexbuf }  (* declare acts as a comment for our IR *)
  | _ as c { raise @@ SyntaxError ("Unexpected character: " ^ Char.escaped c) }

and read_string buf = parse
  | '\\' "00" '"'   { STRING (Buffer.contents buf) }
  | '\\'            { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+   { Buffer.add_string buf (Lexing.lexeme lexbuf)
                    ; read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
