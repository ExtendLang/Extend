type range = string
type cell = range * int * int
type mark_color = White | Grey | Black
exception Cyclic of string

let string_of_cell (rg, r, c) = rg ^ "[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]";;

module CellMap = Map.Make(struct
    type t = cell
    let compare (rng1, row1, col1) (rng2, row2, col2) =
      match Pervasives.compare rng1 rng2 with
        0 -> (match Pervasives.compare row1 row2 with
            0 -> Pervasives.compare col1 col2
          | o -> o)
      | o -> o
  end);;

(* Begin dummy code *)
let dependencies =
  let add_item m c = CellMap.add (fst c) (snd c) m in
  List.fold_left add_item CellMap.empty [
    (*     (cell,    [list of cells it depends on] *)
    (("a", 5, 0),    [("a", 4, 0); ("b", 0, 0)]);
    (("a", 4, 0),    [("a", 3, 0); ("b", 0, 0)]);
    (("a", 3, 0),    [("a", 2, 0); ("b", 0, 0)]);
    (("a", 2, 0),    [("a", 1, 0); ("b", 0, 0)]);
    (("a", 1, 0),    [("a", 0, 0); ("b", 0, 0)]);
    (("a", 0, 0),    [             ("b", 0, 0)]);
  ];;

let get_dep c = try CellMap.find c dependencies with Not_found -> [];;
(* End dummy code *)

let get_color c m = if CellMap.mem c m then CellMap.find c m else White;;

let sort_dependencies c =
  let rec visit (marks, ordering) cell =
    match (get_color cell marks) with
      Grey -> raise (Cyclic (string_of_cell cell))
    | Black -> (marks, ordering)
    | White ->
      let precedents = get_dep cell in
      let (newmarks, newordering) =
        List.fold_left visit (CellMap.add cell Grey marks, ordering) precedents
      in (CellMap.add cell Black newmarks, cell :: newordering)
  in List.rev (snd (visit (CellMap.empty, []) c));;

(* More dummy code - shows usage *)
let deps = sort_dependencies ("a", 5, 0) in
List.iter (fun x -> print_endline (string_of_cell x)) deps;;
