let lexbuf = Lexing.from_channel stdin in
print_endline (Interpreter.interpret lexbuf) ;;
