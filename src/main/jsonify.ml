let jsonify input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_expanded = Transform.expand_expressions ast_raw in
    Ast.string_of_program ast_expanded
