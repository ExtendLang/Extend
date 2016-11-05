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

let dimensions_of_range = function
    Variable(v) -> v.variable_dimensions
  | Subrange(sr) -> sr.subrange_dimensions

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
      (ExtendNumber(n1), ExtendNumber(n2)) -> (match op with
        Plus -> ExtendNumber(n1 + n2)
      | Minus -> ExtendNumber(n1 - n2)
      | Times -> ExtendNumber(n1 * n2)
      | Divide -> ExtendNumber(n1 / n2)
      | _ -> EmptyValue)
    | _ -> EmptyValue in

  let eval_unop op = function
      ExtendNumber(n) -> (match op with
          Neg -> ExtendNumber(-n)
        | _ -> EmptyValue)
    | _ -> EmptyValue in

  let resolve_slice dimension_len cell_index sl =
    let resolve_index = function
        Abs(e) -> let ExtendNumber(i) = (evaluate scope cell e) in i
      | Rel(e) -> let ExtendNumber(i) = (evaluate scope cell e) in cell_index + i
      | DimensionStart -> 0
      | DimensionEnd -> dimension_len in
    (match sl with
       (Some idx1, Some idx2) -> ((resolve_index idx1), (resolve_index idx2))
     | (Some idx, None) -> let i = resolve_index idx in (i, i+1)
     | _ -> raise(Cyclic("Inconceivable!"))) in

  let range_of_val = function
      Range(r) -> r
    | v -> Variable({variable_name = Transform.idgen (); variable_dimensions = Dimensions(1,1); values = ref (CellMap.add (Cell(0,0)) (v, Black) CellMap.empty); formulas=[]}) in

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
  | BinOp(e1, op, e2) -> eval_binop op ((evaluate scope cell e1),(evaluate scope cell e2))
  | UnOp(op, e1) -> eval_unop op (evaluate scope cell e1)
  | Ternary(cond, true_exp, false_exp) -> (match (evaluate scope cell cond) with
        EmptyValue -> EmptyValue
      | ExtendNumber(0) -> (evaluate scope cell false_exp)
      | _ -> (evaluate scope cell true_exp))
  | Id(s) -> Range(Variable(List.find (fun rng -> rng.variable_name = s) scope))
  | Selection(expr, sel) ->
    let rng = range_of_val (evaluate scope cell expr) in
    let Cell(cell_row, cell_col) = cell in
    let Dimensions(rows, cols) = dimensions_of_range rng in
    (match sel with
       (Some row_slice, Some col_slice) ->
       let (row_start, row_end) = resolve_slice rows cell_row row_slice in
       let (col_start, col_end) = resolve_slice rows cell_col col_slice in
       Range(Subrange({base_range = rng; subrange_dimensions = Dimensions(row_end - row_start, col_end - col_start); base_offset = Cell(row_start, col_start)}))
     | _ -> EmptyValue)

(*  LitString of string |
    LitRange of (expr list) list |
    Id of string |
    Wild |
    UnOp of unop * expr |
    Switch of expr option * case list |
    Call of string * expr list |
                     Precedence of expr * expr *)
  | _ -> ExtendNumber(-1))

and get_val scope rg cell =
  match rg with
    Variable(v) -> (
      (* print_endline ("Looking for " ^ v.variable_name ^ index_of_cell cell) ; *)
      let (value, color) = check_val v cell in match color with
        White ->
        let new_value = (evaluate scope cell (get_formula v cell)) in
        (* print_endline ("Finished calculating " ^ v.variable_name ^ index_of_cell cell) ; *)
        v.values := CellMap.add cell (new_value, Black) !(v.values) ; new_value
      | Grey -> let Cell(r, c) = cell in
        raise (Cyclic(v.variable_name ^ "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"))
      | Black ->
        (* print_endline ("Found " ^ v.variable_name ^ index_of_cell cell) ;  *)
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
  | EmptyValue -> "empty"
  | Uncalculated -> "huh?"
  | Range(rg) ->
    let Dimensions(rows, cols) = dimensions_of_range rg in
    let row_list = zero_until rows in
    let col_list = zero_until cols in
    let cart = cartesian row_list col_list in
    "{" ^ (String.concat "; " (tailrec_map (string_of_cell scope rg) cart)) ^ "}"

and string_of_cell scope rg (r,c) =
  index_of_cell (Cell(r,c)) ^ ": " ^ string_of_val scope (get_val scope rg (Cell(r,c)))

let interpret input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.load_imports (Transform.expand_imports ast_raw) in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
  string_of_program ast_expanded ^
  (match ast_expanded with
     (_, _, [foo]) ->
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
     (match (List.filter is_x_vardecl foo.body) with
        [] -> ""
      | [x] ->
        let x_dims = x_dimensions x in
        let rg = {variable_name="x"; variable_dimensions = x_dims; values=ref CellMap.empty; formulas = x_formulas} in
        "\n" ^ string_of_val [rg] (Range(Variable(rg)))
      | x :: xs -> raise (Cyclic("Inconceivable!")))
   | _ -> "");;
