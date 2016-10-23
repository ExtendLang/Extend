let jsonify input =
  let ast = Parser.program Scanner.token input in
    Ast.string_of_program ast
