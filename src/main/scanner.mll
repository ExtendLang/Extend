{
  open Lexing
  open Parser
  open String

  exception SyntaxError of string
  let line_num = ref 1;;
  let char_num = ref 1;;
  let syntax_error token = raise (SyntaxError(
    "Invalid character: " ^ token ^ " on line " ^ (string_of_int !line_num) ^ " at character " ^ (string_of_int !char_num)))
}

let digit = ['0'-'9']
let exp = 'e'('+'|'-')?['0'-'9']+
let flt = (digit)+ ('.' (digit)* exp?|exp)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule token = parse
  ['\n']               { incr line_num; char_num := 1; token lexbuf }
| [' ' '\t' '\r']      { incr char_num; token lexbuf }   (* Whitespace *)
| "/*"                 { char_num := !char_num + 2; multiline_comment lexbuf }
| "//"                 { char_num := !char_num + 2; oneline_comment lexbuf }
| '"'                  { incr char_num; read_string (Buffer.create 17) lexbuf }
| '['             { incr char_num; LSQBRACK }
| ']'             { incr char_num; RSQBRACK }
| '('             { incr char_num; LPAREN }
| ')'             { incr char_num; RPAREN }
| '{'             { incr char_num; LBRACE }
| '}'             { incr char_num; RBRACE }
| ":="            { char_num := !char_num + 2; GETS }
| '='             { incr char_num; ASN }
| ':'             { incr char_num; COLON }
| ','             { incr char_num; COMMA }
| "->"            { char_num := !char_num + 2; PRECEDES }
| '?'             { incr char_num; QUESTION }
| "=="            { char_num := !char_num + 2; EQ }
| "!="            { char_num := !char_num + 2; NOTEQ }
| '<'             { incr char_num; LT }
| '>'             { incr char_num; GT }
| "<="            { char_num := !char_num + 2; LTEQ }
| ">="            { char_num := !char_num + 2; GTEQ }
| ';'             { incr char_num; SEMI }
| '!'             { incr char_num; LOGNOT }
| "&&"            { char_num := !char_num + 2; LOGAND }
| "||"            { char_num := !char_num + 2; LOGOR }
| '~'             { incr char_num; BITNOT }
| '&'             { incr char_num; BITAND }
| '|'             { incr char_num; BITOR }
| '^'             { incr char_num; BITXOR }
| '_'             { incr char_num; UNDERSCORE }
| '+'             { incr char_num; PLUS }
| '-'             { incr char_num; MINUS }
| '*'             { incr char_num; TIMES }
| '/'             { incr char_num; DIVIDE }
| '%'             { incr char_num; MOD }
| "**"            { char_num := !char_num + 2; POWER }
| "<<"            { char_num := !char_num + 2; LSHIFT }
| ">>"            { char_num := !char_num + 2; RSHIFT }
| '#'             { incr char_num; HASH }
| "empty"         { char_num := !char_num + 5; EMPTY }
| "size"          { char_num := !char_num + 4; SIZE }
| "type"          { char_num := !char_num + 4; TYPE }
| "row"           { char_num := !char_num + 3; ROW }
| "column"        { char_num := !char_num + 6; COLUMN }
| "switch"        { char_num := !char_num + 6; SWITCH }
| "case"          { char_num := !char_num + 4; CASE }
| "default"       { char_num := !char_num + 7; DEFAULT }
| "return"        { char_num := !char_num + 6; RETURN }
| "import"        { char_num := !char_num + 6; IMPORT }
| "global"        { char_num := !char_num + 6; GLOBAL }
| "extern"        { char_num := !char_num + 6; EXTERN }
| digit+ as lit   { char_num := (!char_num + (length lit)); LIT_INT(int_of_string lit) }
| flt as lit      { char_num := (!char_num + (length lit)); LIT_FLOAT(float_of_string lit) }
| id as lit       { char_num := (!char_num + (length lit)); ID(lit) }
| eof             { EOF }
| _               { syntax_error (Lexing.lexeme lexbuf) }

and multiline_comment = parse
  "*/" { char_num := !char_num + 2; token lexbuf }
| _    { incr char_num; multiline_comment lexbuf }

and oneline_comment = parse
  '\n' { char_num := 1; token lexbuf }
| _    { incr char_num; oneline_comment lexbuf }

(* read_string mostly taken from:
https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
and read_string buf =
  parse
  | '"'       { incr char_num; LIT_STRING (Buffer.contents buf) }
  | '\\' 'n'  { char_num := !char_num + 3; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { char_num := !char_num + 3; Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { char_num := !char_num + 3; Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' ([^'\\' 'n' 'r' 't'] as lxm)
    { char_num := !char_num + 3; Buffer.add_char buf lxm; read_string buf lexbuf }
  | [^ '"' '\\']+ as lit
    { char_num := (!char_num + (length lit)); Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _         { syntax_error (Lexing.lexeme lexbuf) }
  | eof       { raise (Failure("unterminated string")) }
