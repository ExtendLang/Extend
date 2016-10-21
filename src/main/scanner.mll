{ open Parser }

let digit = ['0'-'9']
let flt = '-'? (digit)+ ('.' (digit)* exp?|exp)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '['             { LSQBRACK }
| ']'             { RSQBRACK }
| '('             { LPAREN }
| ')'             { RPAREN }
| '{'             { LBRACE }
| '}'             { RBRACE }
| ":="            { GETS }
| ':'             { COLON }
| ','             { COMMA }
| '?'             { QUESTION }
| '='             { EQ }
| ';'             { SEMI }
| '_'             { UNDERSCORE }
| '+'             { PLUS }
| '-'             { MINUS }
| '*'             { TIMES }
| '/'             { DIVIDE }
| '%'             { MOD }
| ' '             { EMPTY }
| "switch"        { SWITCH }
| "case"          { CASE }
| "default"       { DEFAULT }
| "return"        { RETURN }
| digit+ as lit   { LITERAL(int_of_string lit) }
| flt as lit      { FLOAT(float_of_string lit) }
| id as lit       { ID(lit) }
| eof             { EOF }
