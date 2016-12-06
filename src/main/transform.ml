open Ast
open Lexing
open Parsing

exception IllegalExpression of string;;
exception DuplicateDefinition of string;;
exception UnknownVariable of string;;
exception UnknownFunction of string;;
exception WrongNumberArgs of string;;
exception LogicError of string;;

let idgen =
  (* from http://stackoverflow.com/questions/10459363/side-effects-and-top-level-expressions-in-ocaml*)
  let count = ref (-1) in
  fun () -> incr count; "_tmp" ^ string_of_int !count;;

module StringSet = Set.Make (String);;
let importSet = StringSet.empty;;

let builtin_signatures = [("cos", 1); ("column", 0); (*("printf", 2);*) ("toString", 1)]

let expand_file filename =
  let print_error_location filename msg lexbuf =
    let pos = lexbuf.lex_curr_p in
    prerr_endline ("Syntax error in \"" ^ filename ^ "\": " ^ msg) ;
    prerr_endline ("Line " ^ (string_of_int pos.pos_lnum) ^ " at character " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))) in

  let rec expand_imports processed_imports globals fns exts = function
      [] -> ([], globals, fns, exts)
    | import :: imports ->
      (* print_endline "--------";
      print_endline ("Working on: " ^ import) ;
      print_endline ("Already processed:"); *)
      (* StringSet.iter (fun a -> print_endline a) processed_imports; *)
      let in_chan = open_in import in
      let lexbuf = (Lexing.from_channel (in_chan)) in
      let (file_imports, file_globals, file_functions, file_externs) =
        try Parser.program Scanner.token lexbuf
        with
          Parsing.Parse_error -> print_error_location import "" lexbuf ; exit(-1)
        | Scanner.SyntaxError(s) -> print_error_location import s lexbuf ; exit(-1)
      in
      let new_proc = StringSet.add import processed_imports and _ = close_in in_chan in
      (* print_endline ("Now I'm done with: ") ; *)
      (* StringSet.iter (fun a -> print_endline a) new_proc; *)
      let first_im_hearing_about imp = not (StringSet.mem imp new_proc || List.mem imp imports) in
      let new_imports = StringSet.elements (StringSet.of_list (List.filter first_im_hearing_about file_imports)) in
      (* print_endline ("First I'm hearing about:") ; *)
      (* List.iter print_endline new_imports; *)
      expand_imports new_proc (globals @ file_globals) (fns @ file_functions) (exts @ file_externs) (imports @ new_imports) in
  expand_imports StringSet.empty [] [] [] [filename]

let expand_expressions (imports, globals, functions, externs) =
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
      Empty        -> raise (IllegalExpression("Empty not allowed " ^ "on LHS")) (* ^ " in " ^ expr_loc)*)
    | Wild         -> raise (IllegalExpression("wild - this shouldn't be possible"))
    | LitString(s) -> raise (IllegalExpression("String literal " ^ quote_string s ^ " not allowed " ^ "on LHS")) (* ^ " in "  ^ expr_loc)*)
    | LitRange(rl) -> raise (IllegalExpression("Range literal " ^ string_of_list (Rows rl) ^ " not allowed on LHS")) (* ^ " in " ^ expr_loc)*)
    | Id(s)        -> (Id(s), [])
    | e            -> let new_id = idgen (* expr_loc *) () in (
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
    (* Expand one or both sides as necessary. *)
      None -> (entire_dimension, [])
    | Some (Some (Abs(e)), None) ->
      let (start_e, start_stmts) = expand_expr e in
      ((Some (Abs(start_e)), None), start_stmts)
    | Some (Some idx_start, Some idx_end) ->
      let (new_start, new_start_exprs) = expand_index idx_start in
      let (new_end, new_end_exprs) = expand_index idx_end in
      ((Some new_start, Some new_end), new_start_exprs @ new_end_exprs)
    | Some (Some _, None) | Some (None, _) -> raise (IllegalExpression("Illegal slice - this shouldn't be possible")) in

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

  let expand_params params =
    let needs_sizevar = function
        ((None, None), _) -> false
      | _ -> true in
    let params_with_sizevar = List.map (fun x -> (idgen(), x)) (List.filter needs_sizevar params) in
    let expanded_args = List.map (fun (sv, ((rv, cv), s)) -> ((sv, s), [((sv, abs_zero), rv); ((sv, abs_one), cv)])) params_with_sizevar in
    let (sizes, inits) = (List.map fst expanded_args, List.concat (List.map snd expanded_args)) in
    let add_item (varset, (assertlist, initlist)) ((sizevar, pos), var) =
      (match var with
         Some Id(s) ->
         if StringSet.mem s varset then
           (* We've seen this variable before; don't initialize it, just assert it *)
           (varset, (BinOp(Id(s), Eq, Selection(Id(sizevar), (Some(Some(pos), None), None))) :: assertlist, initlist))
         else
           (* We're seeing a string for the first time; don't assert it, just create it *)
           (StringSet.add s varset, (assertlist,
                                     Assign(s, zero_comma_zero, Some (Selection(Id(sizevar), (Some(Some(pos), None), None)))) ::
                                     Varinit(one_by_one, [(s, None)]) ::
                                     initlist))
       | Some LitInt(i) -> (* Seeing a number; don't do anything besides create an assertion *)
         (varset, (BinOp(LitInt(i), Eq, Selection(Id(sizevar), (Some(Some(pos), None), None))) :: assertlist, initlist))
       | Some e -> raise (IllegalExpression("Illegal expression (" ^ string_of_expr e ^ ") in function signature"))
       | _ -> raise (IllegalExpression("Cannot supply a single dimension in function signature"))) in
    let (rev_assertions, rev_inits) = snd (List.fold_left add_item (StringSet.empty, ([], [])) inits) in
    let create_sizevar (sizevar,arg) = [
      Varinit(one_by_one, [(sizevar, None)]);
      Assign(sizevar, entire_range, Some(UnOp(SizeOf,Id(arg))))] in
    (List.concat (List.map create_sizevar sizes), List.rev rev_assertions, List.rev rev_inits) in

  let expand_function f =
    let (new_sizevars, assertions, size_inits) = expand_params f.params in
    {
      name = f.name;
      params = f.params;
      raw_asserts = assertions;
      body = new_sizevars @ size_inits @ expand_stmt_list f.body;
      ret_val = f.ret_val
    } in
  (imports, expand_stmt_list globals, List.map expand_function functions, externs);;

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

let create_maps (imports, globals, functions, externs) =
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
     | Assign(var_name, (Some (Some row_start, row_end), Some (Some col_start, col_end)), Some e) ->
       if StringMap.mem var_name m
       then (let v = StringMap.find var_name m in
             StringMap.add var_name {v with var_formulas = v.var_formulas @ [{
                 formula_row_start = row_start;
                 formula_row_end = row_end;
                 formula_col_start = col_start;
                 formula_col_end = col_end;
                 formula_expr = e;
               }]} m)
       else raise (UnknownVariable(string_of_stmt (Assign(var_name, (Some (Some row_start, row_end), Some (Some col_start, col_end)), Some e))))
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
      func_asserts = f.raw_asserts;
    }) in

  let tupleize_library (Library(lib_name, lib_fns)) =
    List.map (fun ext_fn -> (ext_fn.extern_fn_name, {ext_fn with extern_fn_libname = lib_name})) lib_fns in

  (vds_of_stmts globals,
   map_of_list (List.map fd_of_raw_func functions),
   map_of_list (List.concat (List.map tupleize_library externs)))

let check_semantics (globals, functions, externs) =
  let fn_signatures = map_of_list
      (builtin_signatures @
       (StringMap.fold (fun s f l -> (s, List.length f.func_params) :: l) functions []) @
       (StringMap.fold (fun s f l -> (s, List.length f.extern_fn_params) :: l) externs [])) in
  let check_function fname f =
    if StringMap.mem fname externs then raise(DuplicateDefinition(fname ^ "() is defined as both an external and local function")) else ();
    let locals = f.func_body in
    let params = List.map snd f.func_params in
    List.iter
      (fun param ->
         if StringMap.mem param locals then raise(DuplicateDefinition(param ^ " is defined multiple times in " ^ fname ^ "()"))
         else ())
      params ;
    let check_call called_fname num_args =
      match called_fname with
          "printf" -> ()
        | _ -> if (not (StringMap.mem called_fname fn_signatures)) then raise(UnknownFunction(called_fname))
            else let signature_args = StringMap.find called_fname fn_signatures in
            if num_args != signature_args then raise(WrongNumberArgs(
                "In " ^ fname ^ "(), the function " ^ called_fname ^ "() was called with " ^ string_of_int num_args ^ " arguments " ^
                "but the signature specifies " ^ string_of_int signature_args
              ))
            else () in
    let rec check_expr = function
        BinOp(e1,_,e2) -> check_expr e1 ; check_expr e2
      | UnOp(_, e) -> check_expr e
      | Ternary(cond, e1, e2) -> check_expr cond ; check_expr e1 ; check_expr e2
      | Id(s) -> if (List.mem s params || StringMap.mem s locals || StringMap.mem s globals) then () else raise(UnknownVariable(fname ^ "(): " ^ s))
      | Switch(Some e, cases) -> check_expr e ; List.iter check_case cases
      | Switch(None, cases) -> List.iter check_case cases
      | Call(called_fname, args) ->
        check_call called_fname (List.length args) ;
        List.iter check_expr args
      | Selection(e, sel) -> check_expr e ; check_sel sel
      | Precedence(e1, e2) -> check_expr e1 ; check_expr e2
      | LitInt(_) | LitFlt(_) | LitRange(_) | LitString(_) | Empty | Wild -> ()
    and check_case = function
        (Some conds, e) -> List.iter check_expr conds ; check_expr e
      | (None, e) -> check_expr e
    and check_sel = function
        (None, None) -> ()
      | (Some sl1, None) -> check_slice sl1
      | (None, Some sl2) -> check_slice sl2
      | (Some sl1, Some sl2) -> check_slice sl1 ; check_slice sl2
    and check_slice = function
        (None, None) -> ()
      | (Some i1, None) -> check_index i1
      | (None, Some i2) -> check_index i2
      | (Some i1, Some i2) -> check_index i1 ; check_index i2
    and check_index = function
        Abs(e) -> check_expr e
      | Rel(e) -> check_expr e
      | _ -> () in
    let check_formula f =
      check_index f.formula_row_start ;
      (match f.formula_row_end with Some i -> check_index i | None -> ()) ;
      check_index f.formula_col_start ;
      (match f.formula_col_end with Some i -> check_index i | None -> ()) ;
      check_expr f.formula_expr in
    let check_dim = function
        DimInt(_) -> ()
      | DimId(s) -> check_expr (Id(s)) in
    let check_variable v =
      check_dim v.var_rows ;
      check_dim v.var_cols ;
      List.iter check_formula v.var_formulas in

    StringMap.iter (fun _ v -> check_variable v) f.func_body ;
    check_expr (snd f.func_ret_val)

  in StringMap.iter check_function functions

let create_ast filename =
  let ast_imp_res = expand_file filename in
  let ast_expanded = expand_expressions ast_imp_res in
  let ast_mapped = create_maps ast_expanded in
  check_semantics ast_mapped ; ast_mapped
