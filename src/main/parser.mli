type token =
  | LSQBRACK
  | RSQBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | HASH
  | COLON
  | COMMA
  | QUESTION
  | GETS
  | ASN
  | SEMI
  | PRECEDES
  | UNDERSCORE
  | SWITCH
  | CASE
  | DEFAULT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | POWER
  | LSHIFT
  | RSHIFT
  | EQ
  | NOTEQ
  | GT
  | LT
  | GTEQ
  | LTEQ
  | LOGNOT
  | LOGAND
  | LOGOR
  | BITNOT
  | BITXOR
  | BITAND
  | BITOR
  | EMPTY
  | RETURN
  | IMPORT
  | GLOBAL
  | LIT_INT of (int)
  | LIT_FLOAT of (float)
  | LIT_STRING of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
