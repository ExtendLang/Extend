open Ast;;

type cell = Cell of int * int
type dimen = Dimensions of int * int
type mark_color = White | Grey | Black
type cell_value = CellValue of float | EmptyValue | Uncalculated
exception Cyclic of string

module CellMap = Map.Make(struct
    type t = cell
    let compare (Cell(row1, col1)) (Cell(row2, col2)) =
      match Pervasives.compare row1 row2 with
        0 -> Pervasives.compare col1 col2
      | o -> o
  end);;

type range = {
  name: string;
  dimensions: dimen;
  values: (cell_value * mark_color) CellMap.t;
  formulas: assign list;
}

let get_val rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.dimensions in
  if r >= num_rows || r < 0 || c >= num_cols || c < 0 then (EmptyValue, Black) else
    try CellMap.find (Cell(r,c)) rg.values
    with Not_found -> (Uncalculated, White)

let get_formula rg (Cell(r, c)) =
  let Dimensions(num_rows, num_cols) = rg.dimensions in
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
      rg.name = rg_name && r < num_rows && r >= 0 && c < num_cols && c >= 0 &&
      (matchesDim row_start row_end r) && (matchesDim col_start col_end c)
    | _ -> false in
  try (let (_, _, e) = List.find is_match rg.formulas in e) with Not_found -> None

let cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let interpret input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.load_imports (Transform.expand_imports ast_raw) in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
  match ast_expanded with (_, _, [foo]) ->
    let is_x_formula = function
        Assign(s, sel, eo) -> s = "x"
      | _ -> false in
    let x_formula = function
        Assign(a) -> a
      | _ -> raise (Cyclic("Inconceivable!")) in
    let x_formulas = List.map x_formula (List.filter is_x_formula foo.body) in
    let rg = {name="x"; dimensions = Dimensions(5,5); values=CellMap.empty; formulas = x_formulas} in
    let minus_one_through_five = [-1;0;1;2;3;4;5] in
    let cart = cartesian minus_one_through_five minus_one_through_five in
    let string_of_cell (r,c) = "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]: " ^ (
        match (get_formula rg (Cell(r,c))) with
          Some e -> string_of_expr e
        | None -> "undefined") in
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
