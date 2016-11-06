open Ast

exception IllegalExpression of string;;
exception DuplicateDefinition of string;;
exception UnknownVariable of string;;
exception LogicError of string;;

let idgen =
  (* from http://stackoverflow.com/questions/10459363/side-effects-and-top-level-expressions-in-ocaml*)
  let count = ref (-1) in
  fun () -> incr count; "_tmp" ^ string_of_int !count;;

module StringSet = Set.Make (String);;
let importSet = StringSet.empty;;

let expand_imports (imports, globals, functions) =
  let rec find_imports (imports, importSet) =
    let rec find_import (import, importSet) =
      let (imports, globals, functions) = Parser.program Scanner.token (Lexing.from_channel (open_in import))
      in
        find_imports (imports, importSet)
    in
      List.fold_left (fun st item -> if StringSet.mem item st then st else find_import (item, StringSet.add item st)) importSet imports
  in (StringSet.elements (find_imports (imports, StringSet.empty)), globals, functions)

let load_imports (imports, globals, functions) =
  List.fold_left (fun (imp, glo, func) item ->
    let (i, g, f) = Parser.program Scanner.token (Lexing.from_channel (open_in item))
    in (imp, List.append g glo, List.append f func)
  ) ([], globals, functions) imports;;

let expand_expressions (imports, globals, functions) =
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
    | Empty     -> raise (IllegalExpression("Empty"))
    | Wild      -> raise (IllegalExpression("wild - this shouldn't be possible"))
    | e         -> let new_id = idgen() in (
        Id(new_id),
        [Varinit (one_by_one, [(new_id, None)]);
         Assign (new_id, zero_comma_zero, Some e)]) in

  let expand_index = function
    (* Expand one index of a slice if necessary. *)
      Abs(e) -> let (new_e, new_stmts) = expand_expr e in
      (Abs(new_e), new_stmts)
    | DimensionStart -> (DimensionStart, [])
    | DimensionEnd -> (DimensionEnd, [])
    | Rel(_) -> raise (IllegalExpression("relative - this shouldn't be possible")) in

  let expand_slice = function
    (* Turn [(expr)] into [expr:expr+1] if necessary, and
       expand both sides of the slice, if necessary. *)
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

  let expand_assign (var_name, (row_slice, col_slice), formula) =
    (* expand_assign: Take an Assign and return a list of more
       atomic statements, with new variables replacing any
       complex expressions in the selection slices and with single
       index values desugared to expr:expr+1. *)
    try
      let (new_row_slice, row_exprs) = expand_slice row_slice in
      let (new_col_slice, col_exprs) = expand_slice col_slice in
      Assign(var_name, (Some new_row_slice, Some new_col_slice), formula) :: (row_exprs @ col_exprs)
    with IllegalExpression(s) ->
      raise (IllegalExpression("Illegal expression (" ^ s ^ ") in " ^
                               string_of_assign (var_name, (row_slice, col_slice), formula))) in

  let expand_init (r, c) (v, e) =
    Varinit((Some r, Some c), [(v, None)]) ::
    match e with
      None -> []
    | Some e -> [Assign (v, entire_range, Some e)] in

  let expand_dimension = function
      None -> expand_expr (LitInt(1))
    | Some e -> expand_expr e in

  let expand_varinit ((row_dim, col_dim), inits) =
    (* expand_varinit: Take a Varinit and return a list of more atomic
       statements. Each dimension will be given a temporary ID, which
       will be declared as [1,1] _tmpXXX; the formula for tmpXXX will be
       set as a separate assignment; the original variable will be
       declared as [_tmpXXX, _tmpYYY] var; and the formula assignment
       will be applied to [:,:]. *)
    try
      let (row_e, row_stmts) = expand_dimension row_dim in
      let (col_e, col_stmts) = expand_dimension col_dim in
      row_stmts @ col_stmts @ List.concat (List.map (expand_init (row_e, col_e)) inits)
    with IllegalExpression(s) ->
      raise (IllegalExpression("Illegal expression (" ^ s ^ ") in " ^
                               string_of_varinit ((row_dim, col_dim), inits))) in

  let expand_stmt = function
    Assign(a) -> expand_assign(a)
  | Varinit(d, inits) -> expand_varinit (d, inits) in

  let expand_stmt_list stmts = List.concat (List.map expand_stmt stmts) in
  let expand_function f = {
    name = f.name;
    params = f.params;
    body = expand_stmt_list f.body;
    ret_val = f.ret_val} in
  (imports, expand_stmt_list globals, List.map expand_function functions);;

let create_maps (imports, globals, functions) =
  let map_of_list list_of_tuples =
    (*  map_of_list: Take a list of the form [("foo", 2); ("bar", 3)]
        and create a StringMap using the first value of the tuple as
        the key and the second value of the tuple as the value. Raises
        an exception if the key appears more than once in the list. *)
    let rec aux acc = function
        [] -> acc
      | t :: ts ->
        if (StringMap.mem (fst t) acc) then raise(DuplicateDefinition(fst t))
        else aux (StringMap.add (fst t) (snd t) acc) ts in
    aux StringMap.empty list_of_tuples in

  let vd_of_vi = function
    (*  vd_of_vi--- Take a bare Varinit from the previous transformations
        and return a (string, variable) pair    *)
      Varinit((Some r, Some c), [(v, None)]) -> (v, {
        var_rows = (match r with
              LitInt(i) -> DimInt(i)
            | Id(s) -> DimId(s)
            | _ -> raise (LogicError("Unrecognized expression for rows of " ^ v)));
        var_cols = (match c with
              LitInt(i) -> DimInt(i)
            | Id(s) -> DimId(s)
            | _ -> raise (LogicError("Unrecognized expression for rows of " ^ v)));
        var_formulas = [];
      })
    | _ -> raise (LogicError("Unrecognized format for post-desugaring Varinit")) in

  let add_formula m = function
       Varinit(_,_) -> m
     | Assign(var_name, (Some (Some row_start, Some row_end), Some (Some col_start, Some col_end)), Some e) ->
       if StringMap.mem var_name m
       then (let v = StringMap.find var_name m in
             StringMap.add var_name {v with var_formulas = v.var_formulas @ [{
                 formula_row_start = row_start;
                 formula_row_end = row_end;
                 formula_col_start = col_start;
                 formula_col_end = col_end;
                 formula_expr = e;
               }]} m)
       else raise (UnknownVariable(string_of_stmt (Assign(var_name, (Some (Some row_start, Some row_end), Some (Some col_start, Some col_end)), Some e))))
     | Assign(a) -> raise (LogicError("Unrecognized format for post-desugaring Assign: " ^ string_of_stmt (Assign(a)))) in

  let vds_of_stmts stmts =
    let is_varinit = function Varinit(_,_) -> true | _ -> false in
    let varinits = List.filter is_varinit stmts in
    let vars_just_the_names = map_of_list (List.map vd_of_vi varinits) in
    List.fold_left add_formula vars_just_the_names stmts in

  let fd_of_raw_func f = (f.name, {
      func_params = f.params;
      func_body = vds_of_stmts f.body;
      func_ret_val = f.ret_val;
    }) in

  (vds_of_stmts globals, map_of_list (List.map fd_of_raw_func functions))
