open Ast;;

type cell       = Cell of int * int
type dimen      = Dimensions of int * int
type mark_color = White |
                  Grey |
                  Black
exception         Cyclic of string
exception         InvalidIndex of string

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
  interpreter_variable_scope: interpreter_scope;
  values:                    ((cell_value * mark_color) CellMap.t) ref;
}
and  subrange   = {
  base_range:                range;
  base_offset:               cell;
  subrange_dimensions:       dimen;
}
and interpreter_scope = {
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

let __row__ _ (Cell(r, c)) _ = ExtendNumber(r)

let __column__ scope (Cell(r, c)) exprs = ExtendNumber(c)

let builtins = List.fold_left2 (fun m fname f -> StringMap.add fname f m) StringMap.empty ["row";"column"] [__row__;__column__]

let rec evaluate scope cell e =
  let interpreter_variable_of_val v =
    {interpreter_variable_dimensions = Dimensions(1,1);
     interpreter_variable_scope = scope;
     values = ref (CellMap.add (Cell(0,0)) (v, Black) CellMap.empty);
     interpreter_variable_ast_variable = {var_rows = DimInt(1);
                                          var_cols = DimInt(1);
                                          var_formulas = []}} in
  let range_of_val = function
      Range(r) -> r
    | v -> InterpreterVariable(interpreter_variable_of_val v) in

  let eval_binop op = function
      (ExtendNumber(n1), ExtendNumber(n2)) -> (match op with
        Plus -> ExtendNumber(n1 + n2)
      | Minus -> ExtendNumber(n1 - n2)
      | Times -> ExtendNumber(n1 * n2)
      | Divide -> ExtendNumber(n1 / n2)
      | Mod -> ExtendNumber(n1 mod n2)
      | Eq -> if n1 = n2 then ExtendNumber(1) else ExtendNumber(0)
      | Gt -> if n1 > n2 then ExtendNumber(1) else ExtendNumber(0)
      | _ -> EmptyValue)
    | _ -> EmptyValue in

  let eval_unop op v = (match op with
        Neg -> (match v with
            ExtendNumber(n) -> ExtendNumber(-n)
          | _ -> EmptyValue )
      | SizeOf ->
        let Dimensions(r,c) = dimensions_of_range (range_of_val v) in
        Range(InterpreterVariable({interpreter_variable_dimensions = Dimensions(1,2);
                                   interpreter_variable_scope = scope;
                                   values = ref (CellMap.add (Cell(0,0)) (ExtendNumber(r), Black)
                                                   (CellMap.add (Cell(0,1)) (ExtendNumber(c), Black)
                                                      CellMap.empty));
                                   interpreter_variable_ast_variable = {var_rows = DimInt(1);
                                                                        var_cols = DimInt(2);
                                                                        var_formulas = []}}))
      | _ -> EmptyValue) in

  let create_interpreter_variable v =
    let resolve_dimension = function
        DimInt(i) -> i
      | DimId(s) -> (match (evaluate scope (Cell(0,0)) (Id(s))) with
            ExtendNumber(i) -> i
          | _ -> raise (InvalidIndex(s))) in

    let resolve_formula_index dim_length = function
        Abs(e) -> (match (evaluate scope (Cell(0,0)) e) with
            ExtendNumber(i) -> if i >= 0 then Abs(LitInt(i)) else Abs(LitInt(dim_length + i))
          | _ -> raise (InvalidIndex(string_of_expr e)))
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
      interpreter_variable_scope = scope;
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
        Abs(e) -> (match (evaluate scope cell e) with
            ExtendNumber(i) -> if i >= 0 then Some i else Some (dimension_len + i)
          | _ -> None)
      | Rel(e) -> (match (evaluate scope cell e) with
            ExtendNumber(i) -> Some (cell_index + i)
          | _ -> None)
      | DimensionStart -> Some 0
      | DimensionEnd -> Some dimension_len in
    (match sl with
       (Some idx1, Some idx2) -> ((resolve_rhs_index idx1), (resolve_rhs_index idx2))
     | (Some idx, None) -> (match (resolve_rhs_index idx) with
           Some i -> (Some i, Some (i+1))
         | _ -> (None, None))
     | (None, None) -> if dimension_len = 1 then (Some 0, Some 1) else (Some cell_index, Some (cell_index+1))
     | _ -> raise(Cyclic("Inconceivable!"))) in

  let rec val_of_val = function
      Range(rg) ->
      let Dimensions(rows, cols) = dimensions_of_range rg in
      if (rows <= 0) || (cols <= 0) then EmptyValue else
        (if (rows = 1) && (cols = 1) then (val_of_val (get_val rg (Cell(0,0)))) else
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
    let (row_slice, col_slice) = (match sel with
          (Some rs, Some cs) -> (rs, cs)
        | (Some sl, None) -> if rows = 1
          then ((Some(Abs(LitInt(0))),Some(Abs(LitInt(1)))), sl)
          else if cols = 1 then (sl, (Some(Abs(LitInt(0))),Some(Abs(LitInt(1)))))
          else raise(InvalidIndex("Only one slice supplied but neither dimension length is one in " ^ string_of_expr expr))
        | _ -> ((None, None), (None, None))) in
       (match ((resolve_rhs_slice rows cell_row row_slice), (resolve_rhs_slice cols cell_col col_slice)) with
          ((Some row_start, Some row_end), (Some col_start, Some col_end)) ->
          Range(Subrange({base_range = rng;
                          subrange_dimensions = Dimensions(row_end - row_start, col_end - col_start);
                          base_offset = Cell(row_start, col_start)}))
        | _ -> EmptyValue)
  | Call(fname, exprs) ->
    if StringMap.mem fname builtins then
      (StringMap.find fname builtins) scope cell exprs
    else
      let f = StringMap.find fname scope.interpreter_scope_functions in
      let args = List.map (fun e -> interpreter_variable_of_val (evaluate scope (Cell(0,0)) e)) exprs in
      let f_scope = create_scope f args scope.interpreter_scope_functions in
      evaluate f_scope (Cell(0,0)) (snd f.func_ret_val)

(*  LitRange of (expr list) list |
    Switch of expr option * case list |
    Call of string * expr list |
    Precedence of expr * expr *)
  | _ -> ExtendNumber(-1))

and get_val rg cell =
  match rg with
    InterpreterVariable(v) -> (
      (* print_endline ("Looking for " ^ (*v.interpreter_variable_name ^ *) index_of_cell cell) ; *)
      let (value, color) = check_val v cell in match color with
        White ->
        v.values := CellMap.add cell (Uncalculated, Grey) !(v.values) ;
        let new_value = (evaluate v.interpreter_variable_scope cell (get_formula v cell)) in
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
    get_val sr.base_range (Cell(cell_r + sr_r, cell_c + sr_c))

(* and __size__ scope cell exprs =
   *)

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
  | EmptyValue -> "null"
  | Uncalculated -> "huh?"
  | Range(rg) ->
    let Dimensions(rows, cols) = dimensions_of_range rg in
    let row_list = zero_until rows in
    let col_list = zero_until cols in
    let cart = cartesian row_list col_list in
    "[" ^ (String.concat ", " (tailrec_map (string_of_cell scope rg) cart)) ^ "]"

and string_of_cell scope rg (r,c) =
  "{" ^ quote_string (index_of_cell (Cell(r,c))) ^ ": " ^ string_of_val scope (get_val rg (Cell(r,c))) ^ "}"

let interpret ast_mapped =
  try
    (let funcs = snd ast_mapped in
     let mainfunc = StringMap.find "main" funcs in
     let scope = create_scope mainfunc [] funcs in
     (string_of_val scope (evaluate scope (Cell(0,0)) (snd mainfunc.func_ret_val))))
  with Not_found -> "";