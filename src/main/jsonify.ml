let jsonify input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.expand_imports ast_raw in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
    Ast.string_of_program ast_expanded
