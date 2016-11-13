open Ast;;

type cell       = Cell of int * int
type dimen      = Dimensions of int * int
type mark_color = White |
                  Grey |
                  Black
exception         Cyclic of string

let index_of_cell (Cell(r,c)) =
  "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"

module CellMap  = Map.Make(struct
    type t = cell
    let compare (Cell(row1, col1)) (Cell(row2, col2)) =
      match Pervasives.compare row1 row2 with
        0 -> Pervasives.compare col1 col2
      | o -> o
  end);;

type cell_value = ExtendNumber of int |
                  ExtendString of string |
                  Range of range |
                  EmptyValue |
                  Uncalculated
and  range      = InterpreterVariable of interpreter_variable |
                  Subrange of subrange
and  interpreter_variable   = {
  interpreter_variable_ast_variable: variable;
  interpreter_variable_dimensions:       dimen;
  values:                    ((cell_value * mark_color) CellMap.t) ref;
}
and  subrange   = {
  base_range:                range;
  base_offset:               cell;
  subrange_dimensions:       dimen;
}

type interpreter_scope = {
  interpreter_scope_functions: func_decl StringMap.t;
  interpreter_scope_declared_variables: variable StringMap.t;
  interpreter_scope_resolved_variables: (interpreter_variable StringMap.t) ref
}

let dimensions_of_range = function
    InterpreterVariable(v) -> v.interpreter_variable_dimensions
  | Subrange(sr) -> sr.subrange_dimensions

let check_val rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.interpreter_variable_dimensions in
  if r >= num_rows || r < 0 || c >= num_cols || c < 0 then (EmptyValue, Black) else
    try CellMap.find (Cell(r,c)) !(rg.values)
    with Not_found -> (Uncalculated, White)

let create_scope f args function_map =
  let add_argument m arg_name arg_val = StringMap.add arg_name arg_val m in
  let inputs = List.fold_left2 add_argument StringMap.empty (List.map snd f.func_params) args in
   {
     interpreter_scope_functions = function_map;
     interpreter_scope_declared_variables = f.func_body;
     interpreter_scope_resolved_variables = ref inputs;
   }

let get_formula rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.interpreter_variable_dimensions in
  let matchesDim s eo pos =
    let matchMin = match s with
        DimensionStart -> true
      | Abs(LitInt(i)) -> pos >= i
      | _ -> false in
    let matchMax = match eo with
        Some DimensionEnd -> true
      | Some (Abs(LitInt(i))) -> pos < i
      | None -> (match s with
          Abs(LitInt(i)) -> pos = i
          | _ -> false)
      | _ -> false in
    matchMin && matchMax in
  let is_match asn =
    (matchesDim asn.formula_row_start asn.formula_row_end r) &&
    (matchesDim asn.formula_col_start asn.formula_col_end c) in
  let maybe_formula =
    if r < num_rows && r >= 0 && c < num_cols && c >= 0 then
    (try
      let asn = List.find is_match rg.interpreter_variable_ast_variable.var_formulas in
      Some asn.formula_expr
     with Not_found -> None)
    else None in
  match maybe_formula with
    Some e -> e
  | None -> Empty

let rec evaluate scope cell e =
  let eval_binop op = function
      (ExtendNumber(n1), ExtendNumber(n2)) -> (match op with
        Plus -> ExtendNumber(n1 + n2)
      | Minus -> ExtendNumber(n1 - n2)
      | Times -> ExtendNumber(n1 * n2)
      | Divide -> ExtendNumber(n1 / n2)
      | Eq -> if n1 = n2 then ExtendNumber(1) else ExtendNumber(0)
      | Gt -> if n1 > n2 then ExtendNumber(1) else ExtendNumber(0)
      | _ -> EmptyValue)
    | _ -> EmptyValue in

  let eval_unop op = function
      ExtendNumber(n) -> (match op with
          Neg -> ExtendNumber(-n)
        | _ -> EmptyValue)
    | _ -> EmptyValue in

  let create_interpreter_variable v =
    let resolve_dimension = function
        DimInt(i) -> i
      | DimId(s) -> let ExtendNumber(i) = (evaluate scope (Cell(0,0)) (Id(s))) in i in

    let resolve_formula_index dim_length = function
        Abs(e) -> let ExtendNumber(i) = (evaluate scope (Cell(0,0)) e) in
        if i >= 0 then Abs(LitInt(i)) else Abs(LitInt(dim_length + i))
      | o -> o in

    let resolve_formula (Dimensions(r, c)) f = {
      formula_row_start = resolve_formula_index r f.formula_row_start;
      formula_row_end = (match f.formula_row_end with
            Some e -> Some (resolve_formula_index r e)
          | None -> None);
      formula_col_start = resolve_formula_index c f.formula_col_start;
      formula_col_end = (match f.formula_col_end with
            Some e -> Some (resolve_formula_index c e)
          | None -> None);
      formula_expr = f.formula_expr;
    } in

    let new_dimensions = Dimensions(resolve_dimension v.var_rows, resolve_dimension v.var_cols) in
    {
      interpreter_variable_dimensions = new_dimensions;
      interpreter_variable_ast_variable = {v with var_formulas = List.map (resolve_formula new_dimensions) v.var_formulas};
      values = ref CellMap.empty;
    } in

  let find_variable s =
    let v =
      if StringMap.mem s !(scope.interpreter_scope_resolved_variables)
      then StringMap.find s !(scope.interpreter_scope_resolved_variables)
      else
        let new_var = create_interpreter_variable (StringMap.find s scope.interpreter_scope_declared_variables) in
        scope.interpreter_scope_resolved_variables := StringMap.add s new_var !(scope.interpreter_scope_resolved_variables) ;
        new_var in
    Range(InterpreterVariable(v)) in

  let resolve_rhs_slice dimension_len cell_index sl =
    let resolve_rhs_index = function
        Abs(e) -> let ExtendNumber(i) = (evaluate scope cell e) in
        if i >= 0 then i else dimension_len + i
      | Rel(e) -> let ExtendNumber(i) = (evaluate scope cell e) in cell_index + i
      | DimensionStart -> 0
      | DimensionEnd -> dimension_len in
    (match sl with
       (Some idx1, Some idx2) -> ((resolve_rhs_index idx1), (resolve_rhs_index idx2))
     | (Some idx, None) -> let i = resolve_rhs_index idx in (i, i+1)
     | _ -> raise(Cyclic("Inconceivable!"))) in

  let interpreter_variable_of_val v =
    {interpreter_variable_dimensions = Dimensions(1,1);
     values = ref (CellMap.add (Cell(0,0)) (v, Black) CellMap.empty);
     interpreter_variable_ast_variable = {var_rows = DimInt(1);
                                          var_cols = DimInt(1);
                                          var_formulas = []}} in

  let range_of_val = function
      Range(r) -> r
    | v -> InterpreterVariable(interpreter_variable_of_val v) in

  let rec val_of_val = function
      Range(rg) ->
      let Dimensions(rows, cols) = dimensions_of_range rg in
      if (rows <= 0) || (cols <= 0) then EmptyValue else
        (if (rows = 1) && (cols = 1) then (val_of_val (get_val scope rg (Cell(0,0)))) else
           Range(rg))
    | o -> o in

  val_of_val (match e with
    Empty -> EmptyValue
  | LitInt(i) -> ExtendNumber(i)
  | LitFlt(f) -> ExtendNumber(int_of_float f)
  | LitString(s) -> ExtendString(s)
  | BinOp(e1, op, e2) -> eval_binop op ((evaluate scope cell e1),(evaluate scope cell e2))
  | UnOp(op, e1) -> eval_unop op (evaluate scope cell e1)
  | Ternary(cond, true_exp, false_exp) -> (match (evaluate scope cell cond) with
        EmptyValue -> EmptyValue
      | ExtendNumber(0) -> (evaluate scope cell false_exp)
      | _ -> (evaluate scope cell true_exp))
  | Id(s) -> find_variable s
  | Selection(expr, sel) ->
    let rng = range_of_val (evaluate scope cell expr) in
    let Cell(cell_row, cell_col) = cell in
    let Dimensions(rows, cols) = dimensions_of_range rng in
    (match sel with
       (Some row_slice, Some col_slice) ->
       let (row_start, row_end) = resolve_rhs_slice rows cell_row row_slice in
       let (col_start, col_end) = resolve_rhs_slice cols cell_col col_slice in
       Range(Subrange({base_range = rng; subrange_dimensions = Dimensions(row_end - row_start, col_end - col_start); base_offset = Cell(row_start, col_start)}))
     | _ -> EmptyValue)
  | Call(fname, exprs) ->
    let f = StringMap.find fname scope.interpreter_scope_functions in
    let args = List.map (fun e -> interpreter_variable_of_val (evaluate scope (Cell(0,0)) e)) exprs in
    let f_scope = create_scope f args scope.interpreter_scope_functions in
    evaluate f_scope (Cell(0,0)) (snd f.func_ret_val)

(*  LitRange of (expr list) list |
    Switch of expr option * case list |
    Call of string * expr list |
    Precedence of expr * expr *)
  | _ -> ExtendNumber(-1))

and get_val scope rg cell =
  match rg with
    InterpreterVariable(v) -> (
      (* print_endline ("Looking for " ^ v.interpreter_variable_name ^ index_of_cell cell) ; *)
      let (value, color) = check_val v cell in match color with
        White ->
        let new_value = (evaluate scope cell (get_formula v cell)) in
        (* print_endline ("Finished calculating " ^ v.interpreter_variable_name ^ index_of_cell cell) ; *)
        v.values := CellMap.add cell (new_value, Black) !(v.values) ; new_value
      | Grey -> let Cell(r, c) = cell in
        raise (Cyclic("[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"))
      | Black ->
        (* print_endline ("Found " ^ v.interpreter_variable_name ^ index_of_cell cell) ;  *)
        value)
  | Subrange(sr) ->
    let Cell(cell_r, cell_c) = cell in
    let Cell(sr_r, sr_c) = sr.base_offset in
    get_val scope sr.base_range (Cell(cell_r + sr_r, cell_c + sr_c))


(* All stub code below this point *)

(* from http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function without the infix *)
let zero_until i =
  let rec aux n acc =
    if n < 0 then acc else aux (n-1) (n :: acc)
  in aux (i-1) []

(* from http://stackoverflow.com/questions/27930976/how-to-make-the-cartesian-product-of-two-lists-in-ocaml *)
(* This is the tail-recursive version *)
let cartesian l l' =
  List.rev (List.fold_left (fun x a -> List.fold_left (fun y b -> (a,b) :: y) x l') [] l)

(* from http://stackoverflow.com/questions/27386520/tail-recursive-list-map *)

let tailrec_map f l =
  let rec map_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] l

let rec string_of_val scope = function
    ExtendNumber(cv) -> string_of_int cv
  | ExtendString(s) -> quote_string s
  | EmptyValue -> "empty"
  | Uncalculated -> "huh?"
  | Range(rg) ->
    let Dimensions(rows, cols) = dimensions_of_range rg in
    let row_list = zero_until rows in
    let col_list = zero_until cols in
    let cart = cartesian row_list col_list in
    "[" ^ (String.concat ", " (tailrec_map (string_of_cell scope rg) cart)) ^ "]"

and string_of_cell scope rg (r,c) =
  "{" ^ quote_string (index_of_cell (Cell(r,c))) ^ ": " ^ quote_string (string_of_val scope (get_val scope rg (Cell(r,c)))) ^ "}"

let interpret input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.load_imports (Transform.expand_imports ast_raw) in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
  let ast_mapped = Transform.create_maps ast_expanded in
  string_of_program ast_mapped ^ try
    (let funcs = snd ast_mapped in
     let mainfunc = StringMap.find "main" funcs in
     let scope = create_scope mainfunc [] funcs in
     "\n" ^ (string_of_val scope (evaluate scope (Cell(0,0)) (snd mainfunc.func_ret_val))))
  with Not_found -> "";
