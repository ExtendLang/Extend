open Ast

exception IllegalExpression of string;;
exception DuplicateDefinition of string;;
exception UnknownVariable of string;;
exception UnknownFunction of string;;
exception WrongNumberArgs of string;;
exception LogicError of string;;

type symbol = LocalVariable of int | GlobalVariable of int | FunctionParameter of int | ExtendFunction of int
and  symbolTable = symbol StringMap.t
and  symbolTableType = Locals | Globals | ExtendFunctions

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
  aux StringMap.empty list_of_tuples

let index_map table_type m =
  let add_item key _ (accum_map, accum_idx) =
    let index_val = match table_type with Locals -> LocalVariable(accum_idx) | Globals -> GlobalVariable(accum_idx) | ExtendFunctions -> ExtendFunction(accum_idx) in
    (StringMap.add key index_val accum_map, accum_idx + 1) in
  StringMap.fold add_item m (StringMap.empty, 0)

let create_symbol_table global_symbols fn_def =
  let (local_indices, _) = index_map Locals fn_def.func_body in
  let add_param (st, idx) param_name =
    let new_st = StringMap.add param_name (FunctionParameter(idx)) st in
    (new_st, idx + 1) in
  let (params_and_globals, _) = List.fold_left add_param (global_symbols, 0) (List.map snd fn_def.func_params) in
  StringMap.fold StringMap.add local_indices params_and_globals

let check_semantics (globals, functions, externs) =
  let fn_signatures = map_of_list
      ((StringMap.fold (fun s f l -> (s, List.length f.func_params) :: l) functions []) @
       (StringMap.fold (fun s f l -> (s, List.length f.extern_fn_params) :: l) externs [])) in
  let (global_symbols, _) = index_map Globals globals in

  let check_call context called_fname num_args =
    if (not (StringMap.mem called_fname fn_signatures)) then
      (print_endline ("In " ^ context ^ "(), the undefined function " ^ called_fname ^ "() was called") ;
       raise(UnknownFunction(context ^ "," ^ called_fname)))
    else let signature_args = StringMap.find called_fname fn_signatures in
      if num_args != signature_args then
        (print_endline ("In " ^ context ^ "(), the function " ^ called_fname ^ "() was called with " ^
                       string_of_int num_args ^ " arguments " ^ "but the signature specifies "
                       ^ string_of_int signature_args) ;
         raise(WrongNumberArgs(context ^ "," ^ called_fname)))
      else () in

  let rec check_expr fname symbols = function
      BinOp(e1,_,e2) -> check_expr fname symbols e1 ; check_expr fname symbols e2
    | UnOp(_, e) -> check_expr fname symbols e
    | Ternary(cond, e1, e2) -> check_expr fname symbols cond ; check_expr fname symbols e1 ; check_expr fname symbols e2
    | ReducedTernary(s1, s2, s3) -> check_expr fname symbols (Id(s1)) ; check_expr fname symbols (Id(s2)) ; check_expr fname symbols (Id(s3))
    | Id(s) -> if StringMap.mem s symbols then () else raise(UnknownVariable(fname ^ "(): " ^ s))
    | Switch(Some e, cases, dflt) -> check_expr fname symbols e ; List.iter (fun c -> check_case fname symbols c) cases ; check_expr fname symbols dflt
    | Switch(None, cases, dflt) -> List.iter (fun c -> check_case fname symbols c) cases ; check_expr fname symbols dflt
    | Call(called_fname, args) ->
      check_call fname called_fname (List.length args) ;
      List.iter (fun a -> check_expr fname symbols a) args
    | Selection(e, (sl1, sl2)) -> check_expr fname symbols e ; check_slice fname symbols sl1 ; check_slice fname symbols sl2
    | Precedence(e1, e2) -> check_expr fname symbols e1 ; check_expr fname symbols e2
    | Debug(e) -> check_expr fname symbols e;
    | LitInt(_) | LitFlt(_) | LitRange(_) | LitString(_) | Empty | Wild -> ()
  and check_case fname symbols (conds, e) = List.iter (fun c -> check_expr fname symbols c) conds ; check_expr fname symbols e
  and check_slice fname symbols = function
      None -> ()
    | Some (i1, i2) -> check_index fname symbols i1 ; check_index fname symbols i2
  and check_index fname symbols = function
      Some Abs(e) -> check_expr fname symbols e
    | Some Rel(e) -> check_expr fname symbols e
    | _ -> () in
  let check_formula fname symbols f =
    check_index fname symbols (Some f.formula_row_start) ;
    check_index fname symbols f.formula_row_end ;
    check_index fname symbols (Some f.formula_col_start) ;
    check_index fname symbols f.formula_col_end ;
    check_expr fname symbols f.formula_expr in
  let check_dim fname symbols = function
      DimInt(1) -> ()
    | DimInt(i) -> raise(IllegalExpression("This is not going to work right"))
    | DimId(s) -> check_expr fname symbols (Id(s)) in
  let check_variable fname symbols v =
    check_dim fname symbols v.var_rows ;
    check_dim fname symbols v.var_cols ;
    List.iter (fun f -> check_formula fname symbols f) v.var_formulas in
  let check_variables context symbols vars =
    StringMap.iter (fun _ v -> check_variable context symbols v) vars in

  let check_function fname f =
    if StringMap.mem fname externs then raise(DuplicateDefinition(fname ^ "() is defined as both an external and local function")) else ();
    let locals = f.func_body in
    let params = List.map snd f.func_params in
    List.iter
      (fun param ->
         if StringMap.mem param locals then raise(DuplicateDefinition(param ^ " is defined multiple times in " ^ fname ^ "()"))
         else ())
      params ;
    let local_symbols = create_symbol_table global_symbols f in
    check_variables fname local_symbols f.func_body ;
    check_expr fname local_symbols (snd f.func_ret_val)

  in check_variables "global_variables" global_symbols globals ; StringMap.iter check_function functions
