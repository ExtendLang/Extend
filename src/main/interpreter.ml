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

type cell_value = ExtendNumber of float |
                  Range of range |
                  EmptyValue |
                  Uncalculated
and  range      = Variable of variable |
                  Subrange of subrange
and  variable   = {
  variable_name:             string;
  variable_dimensions:       dimen;
  values:                    ((cell_value * mark_color) CellMap.t) ref;
  formulas:                  assign list;
}
and  subrange   = {
  base_range:                range;
  base_offset:               cell;
  subrange_dimensions:       dimen;
}

let check_val rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.variable_dimensions in
  if r >= num_rows || r < 0 || c >= num_cols || c < 0 then (EmptyValue, Black) else
    try CellMap.find (Cell(r,c)) !(rg.values)
    with Not_found -> (Uncalculated, White)

let get_formula rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.variable_dimensions in
  let matchesDim s e pos =
    let matchMin = match s with
        DimensionStart -> true
      | Abs(LitInt(i)) -> pos >= i
      | _ -> false in
    let matchMax = match e with
        DimensionEnd -> true
      | Abs(LitInt(i)) -> pos < i
      | _ -> false in
    matchMin && matchMax in
  let is_match asn =
    match asn with
      (rg_name, (Some (Some row_start, Some row_end), Some (Some col_start, Some col_end)), Some e) ->
      rg.variable_name = rg_name && r < num_rows && r >= 0 && c < num_cols && c >= 0 &&
      (matchesDim row_start row_end r) && (matchesDim col_start col_end c)
    | _ -> false in
  let maybe_formula = try (let (_, _, e) = List.find is_match rg.formulas in e) with Not_found -> None in
  match maybe_formula with
    Some e -> e
  | None -> Empty

let rec evaluate scope cell e =
  let eval_binop op = function
      (ExtendNumber(f1), ExtendNumber(f2)) -> (match op with
        Plus -> ExtendNumber(f1 +. f2)
      | Minus -> ExtendNumber(f1 -. f2)
      | Times -> ExtendNumber(f1 *. f2)
      | Divide -> ExtendNumber(f1 /. f2)
      | _ -> EmptyValue)
    | _ -> EmptyValue in
  match e with
    Empty -> EmptyValue
  | LitInt(i) -> ExtendNumber(float_of_int i)
  | LitFlt(f) -> ExtendNumber(f)
  | BinOp(e1, op, e2) -> eval_binop op ((evaluate scope cell e1),(evaluate scope cell e2))
  | Ternary(cond, true_exp, false_exp) -> (match (evaluate scope cell cond) with
        EmptyValue -> EmptyValue
      | ExtendNumber(0.) -> (evaluate scope cell false_exp)
      | _ -> (evaluate scope cell true_exp))
  | Id(s) -> Range(Variable(List.find (fun rng -> rng.variable_name = s) scope))
  | Selection(expr, sel) ->
    let Range(Variable(rng)) = (evaluate scope cell expr) in
    let Cell(formula_row, formula_col) = cell in
    let resolve_index formula_index = function
        Abs(LitInt(i)) -> i
      | Rel(LitInt(i)) -> formula_index + i
      | Rel(UnOp(Neg,LitInt(i))) -> formula_index - i
      | _ -> raise(Cyclic("Inconceivable!")) in
    get_val scope rng (match sel with
          (Some (Some row_idx, None), Some (Some col_idx, None)) -> Cell(resolve_index formula_row row_idx, resolve_index formula_col col_idx)
      )
(*                   LitFlt of float |
    LitString of string |
    LitRange of (expr list) list |
    Id of string |
    Wild |
    UnOp of unop * expr |
    Switch of expr option * case list |
    Call of string * expr list |
    Selection of expr * sel |
    Precedence of expr * expr *)
  | _ -> ExtendNumber(3.14159)


and get_val scope rg cell =
  print_endline ("Looking for " ^ rg.variable_name ^ index_of_cell cell) ;
  let (value, color) = check_val rg cell in match color with
    White ->
    let new_value = (evaluate scope cell (get_formula rg cell)) in
    print_endline ("Finished calculating " ^ rg.variable_name ^ index_of_cell cell) ;
    rg.values := CellMap.add cell (new_value, Black) !(rg.values) ; new_value
  | Grey -> let Cell(r, c) = cell in
    raise (Cyclic(rg.variable_name ^ "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"))
  | Black -> print_endline ("Found " ^ rg.variable_name ^ index_of_cell cell) ; value

(* from http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function without the infix *)
let zero_until i =
  let rec aux n acc =
    if n < 0 then acc else aux (n-1) (n :: acc)
  in aux (i-1) []

(* from http://stackoverflow.com/questions/27930976/how-to-make-the-cartesian-product-of-two-lists-in-ocaml *)
let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let interpret input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.load_imports (Transform.expand_imports ast_raw) in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
  print_endline (string_of_program ast_expanded) ;
  match ast_expanded with (_, _, [foo]) ->
    let is_x_formula = function
        Assign(s, sel, eo) -> s = "x"
      | _ -> false in
    let x_formula = function
        Assign(a) -> a
      | _ -> raise (Cyclic("Inconceivable!")) in
    let is_x_vardecl = function
        Vardecl(_, [(s, None)]) -> s = "x"
      | _ -> false in
    let x_dimensions = function
        Vardecl((Some(LitInt(rows)), (Some(LitInt(cols)))), _) -> Dimensions(rows, cols)
      | _ -> raise (Cyclic("Inconceivable!")) in
    let x_formulas = List.map x_formula (List.filter is_x_formula foo.body) in
    let x_dims = x_dimensions (List.hd (List.filter is_x_vardecl foo.body)) in
    let rg = {variable_name="x"; variable_dimensions = x_dims; values=ref CellMap.empty; formulas = x_formulas} in
    let Dimensions(rows, cols) = x_dims in
    let row_list = zero_until rows in let col_list = zero_until cols in
    let cart = cartesian row_list col_list in
    let string_of_cell (r,c) = "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]: " ^ (match get_val [rg] rg (Cell(r,c)) with
          ExtendNumber(cv) -> string_of_float cv |
          EmptyValue -> "empty" |
          Uncalculated -> "huh?" |
          Range(_) -> "some range here"
      ) in
    String.concat "\n" (List.map string_of_cell cart)
  | _ -> "";;

(*


let rg = {
  name = "mine";
  dimensions = Dimensions(1,1);
  values = CellMap.empty;
  formulas = [];
}

let rg = {rg with values = CellMap.add (Cell(0,0)) (CellValue(3.14159), Black) rg.values};;

let (value, color) = get_val rg (Cell(2,0)) in match color with
  White -> print_endline "i don't know yet"
| Grey -> print_endline "hold your horses, i'm still working"
| Black -> match value with
    CellValue(f) -> print_endline (string_of_float f)
  | EmptyValue -> print_endline "empty"
  | Uncalculated -> print_endline "i don't know yet"
*)
