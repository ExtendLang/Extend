let lexbuf = Lexing.from_channel stdin in
print_endline (Llvm.string_of_llmodule (Codegen.build_this lexbuf)) ;;
