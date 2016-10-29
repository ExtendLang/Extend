open Ast

let idgen =
  (* from http://stackoverflow.com/questions/10459363/side-effects-and-top-level-expressions-in-ocaml*)
  let count = ref (-1) in
  fun () -> incr count; "_tmp" ^ string_of_int !count;;

let expand_expressions (imports, globals, functions) =
  (* expand_dimensions: Take a Vardecl and return a list of
     more atomic statements. Each dimension will be given a
     temporary ID, which will be declared as [1,1] _tmpXXX;
     the formula for tmpXXX will be set as a separate assignment;
     the original variable will be declared as [_tmpXXX, _tmpYYY] var;
     and the formula assignment will be applied to [0:_tmpXXX,0:_tmpYYY].
  *)
  let expand_stmt s =
    let expand_dimensions (row_dim, col_dim) inits =
      let expand_init (r, c) (v, e) =
        Vardecl((Some (Id(r)), Some (Id(c))), [(v, None)]) ::
        match e with
          None -> []
        | Some e -> [Assign (v,
                             (Some (Some (Abs(LitInt(0))), Some (Abs(Id(r)))),
                              Some (Some (Abs(LitInt(0))), Some (Abs(Id(c))))),
                             Some e)] in
      let expand_dimension = function
          None -> (idgen (), LitInt(1))
        | Some expr -> (idgen (), expr) in
      let (row_var, row_expr) = expand_dimension row_dim in
      let (col_var, col_expr) = expand_dimension col_dim in
      Vardecl ((Some (LitInt(1)), Some (LitInt(1))), [(row_var, None)]) ::
      Vardecl ((Some (LitInt(1)), Some (LitInt(1))), [(col_var, None)]) ::
      Assign (row_var,
              (Some (Some (Abs(LitInt(0))), Some (Abs(LitInt(1)))),
               Some (Some (Abs(LitInt(0))), Some (Abs(LitInt(1))))),
              Some row_expr) ::
      Assign (col_var,
              (Some (Some (Abs(LitInt(0))), Some (Abs(LitInt(1)))),
               Some (Some (Abs(LitInt(0))), Some (Abs(LitInt(1))))),
              Some col_expr) ::
      List.concat (List.map (expand_init (row_var, col_var)) inits) in
    match s with
      Assign(a) -> [Assign(a)]
    | Vardecl(d, inits) -> expand_dimensions d inits in
  let expand_function f = {
    name = f.name;
    params = f.params;
    body = List.concat (List.map expand_stmt f.body);
    ret_val = f.ret_val} in
  (imports, globals, List.map expand_function functions);;
