open Ast

exception Wild_encounter of string;

let idgen =
  (* from http://stackoverflow.com/questions/10459363/side-effects-and-top-level-expressions-in-ocaml*)
  let count = ref (-1) in
  fun () -> incr count; "_tmp" ^ string_of_int !count;;

let expand_expressions (imports, globals, functions) =
  let expand_stmt s =
    let lit_zero = LitInt(0) in let abs_zero = Abs(lit_zero) in
    let lit_one  = LitInt(1) in let abs_one  = Abs(lit_one)  in
    let one_by_one = (Some lit_one, Some lit_one) in
    let zero_comma_zero = (Some (Some abs_zero, Some abs_one),
                           Some (Some abs_zero, Some abs_one)) in
    let entire_dimension = (Some DimensionStart, Some DimensionEnd) in
    let entire_range = (Some entire_dimension, Some entire_dimension) in
    let expand_expr = function
      (* If expression is not sufficiently atomic, create a new variable
         to hold the expression; return the new expression and whatever
         new statements are necessary to create the new variable *)
        LitInt(i) -> (LitInt(i), [])
      | Id(s)     -> (Id(s), [])
      | Empty     -> (Empty, []) (* This should be a semantic error *)
      | Wild      -> raise (Wild_encounter "Encountered Wild expression, this should not happen, please check your code")
      | e         -> let new_id = idgen() in (
          Id(new_id),
          [Vardecl (one_by_one, [(new_id, None)]);
           Assign (new_id, zero_comma_zero, Some e)])
    in
    let expand_assign (var_name, (row_slice, col_slice), formula) =
      (* expand_assign: Take an Assign and return a list of more
         atomic statements, with new variables replacing any
         complex expressions in the selection slices and with single
         index values desugared to expr:expr+1. *)
      let expand_index = function
          Abs(e) -> let (new_e, new_stmts) = expand_expr e in
          (Abs(new_e), new_stmts)
        | DimensionStart -> (DimensionStart, [])
        | DimensionEnd -> (DimensionEnd, [])
        | _ -> (Abs(Empty), []) raise (Wild_encounter "Unexpected input for dimension") in
      let expand_slice = function
          None -> (entire_dimension, [])
        | Some (Some (Abs(e)), None) ->
          let (start_e, start_stmts) = expand_expr e in
          let (end_e, end_stmts) = expand_expr (BinOp(start_e, Plus, lit_one)) in
          ((Some (Abs(start_e)), Some (Abs(end_e))), start_stmts @ end_stmts)
        | Some (Some idx_start, Some idx_end) ->
          let (new_start, new_start_exprs) = expand_index idx_start in
          let (new_end, new_end_exprs) = expand_index idx_end in
          ((Some new_start, Some new_end), new_start_exprs @ new_end_exprs)
        | Some (Some _, None) | Some (None, _) -> ((None, None), []) in (* TODO: RAISE AN EXCEPTION!!! *)
      let (new_row_slice, row_exprs) = expand_slice row_slice in
      let (new_col_slice, col_exprs) = expand_slice col_slice

      in (* Return value for expand_assign: *)
      Assign(var_name, (Some new_row_slice, Some new_col_slice), formula) ::
      (row_exprs @ col_exprs)
    in
    let expand_vardecl ((row_dim, col_dim), inits) =
      (* expand_vardecl: Take a Vardecl and return a list of more atomic
         statements. Each dimension will be given a temporary ID, which
         will be declared as [1,1] _tmpXXX; the formula for tmpXXX will be
         set as a separate assignment; the original variable will be
         declared as [_tmpXXX, _tmpYYY] var; and the formula assignment
         will be applied to [:,:]. *)
      let expand_init (r, c) (v, e) =
        Vardecl((Some r, Some c), [(v, None)]) ::
        match e with
          None -> []
        | Some e -> [Assign (v, entire_range, Some e)] in
      let expand_dimension = function
          None -> expand_expr (LitInt(1))
        | Some e -> expand_expr e in
      let (row_e, row_stmts) = expand_dimension row_dim in
      let (col_e, col_stmts) = expand_dimension col_dim

      in (* Return expression for expand_vardecl: *)
      row_stmts @ col_stmts @
      List.concat (List.map (expand_init (row_e, col_e)) inits)

    in (* Return expression for expand_stmt: *)
    match s with
      Assign(a) -> expand_assign(a)
    | Vardecl(d, inits) -> expand_vardecl (d, inits) in

  let expand_stmt_list stmts = List.concat (List.map expand_stmt stmts) in
  let expand_function f = {
    name = f.name;
    params = f.params;
    body = expand_stmt_list f.body;
    ret_val = f.ret_val} in
  (imports, expand_stmt_list globals, List.map expand_function functions);;
