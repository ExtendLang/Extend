let lexbuf = Lexing.from_channel stdin in
let ast = Parser.program Scanner.token lexbuf in
print_endline (Ast.string_of_program ast) ;;
