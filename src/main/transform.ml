(* jss2272 *)

open Ast
open Lexing
open Parsing
open Semant

module StringSet = Set.Make (String);;
let importSet = StringSet.empty;;

let idgen =
  (* from http://stackoverflow.com/questions/10459363/side-effects-and-top-level-expressions-in-ocaml*)
  let count = ref (-1) in
  fun prefix -> incr count; "_tmp_" ^ prefix ^ string_of_int !count;;

let expand_file include_stdlib filename =
  let print_error_location filename msg lexbuf =
    let pos = lexbuf.lex_curr_p in
    prerr_endline ("Syntax error in \"" ^ filename ^ "\": " ^ msg) ;
    prerr_endline ("Line " ^ (string_of_int pos.pos_lnum) ^ " at character " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))) in

  let rec expand_imports processed_imports globals fns exts dir = function
      [] -> ([], globals, fns, exts)
    | (import, use_dir) :: imports ->
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
      let file_imports = List.map (fun file -> (if use_dir then (dir ^ "/") else "") ^ file) file_imports in
      let new_proc = StringSet.add import processed_imports and _ = close_in in_chan in
      (* print_endline ("Now I'm done with: ") ; *)
      (* StringSet.iter (fun a -> print_endline a) new_proc; *)
      let first_im_hearing_about imp = not (StringSet.mem imp new_proc || List.mem imp (List.map fst imports)) in
      let new_imports = List.map (fun e -> (e, true)) (StringSet.elements (StringSet.of_list (List.filter first_im_hearing_about file_imports))) in
      (* print_endline ("First I'm hearing about:") ; *)
      (* List.iter print_endline new_imports; *)
      expand_imports new_proc (globals @ file_globals) (fns @ file_functions) (exts @ file_externs) (Filename.dirname import) (imports @ new_imports) in
  expand_imports
    StringSet.empty [] [] []
    (Filename.dirname filename)
    (if include_stdlib then [(filename, true); ("src/stdlib/stdlib.xtnd", false)] else [(filename, true)])

let expand_expressions (imports, globals, functions, externs) =
  let lit_zero = LitInt(0) in let abs_zero = Abs(lit_zero) in
  let lit_one  = LitInt(1) in let abs_one  = Abs(lit_one)  in
  let one_by_one = (Some lit_one, Some lit_one) in
  let zero_comma_zero = (Some (Some abs_zero, Some abs_one),
                         Some (Some abs_zero, Some abs_one)) in
  let entire_dimension = (Some DimensionStart, Some DimensionEnd) in
  let entire_range = (Some entire_dimension, Some entire_dimension) in

  let expand_expr expr_loc = function
    (* Create a new variable for all expressions on the LHS to hold the result;
       return the new expression and whatever new statements are necessary to create the new variable *)
      Empty     -> raise (IllegalExpression("Empty not allowed in " ^ expr_loc))
    | LitString(s) -> raise (IllegalExpression("String literal " ^ quote_string s ^ " not allowed in " ^ expr_loc))
    | LitRange(rl) -> raise (IllegalExpression("Range literal " ^ string_of_list (Rows rl) ^ " not allowed in " ^ expr_loc))
    | e         -> let new_id = idgen expr_loc in (
        Id(new_id),
        [Varinit (one_by_one, [(new_id, None)]);
         Assign (new_id, zero_comma_zero, Some e)]) in

  let expand_index index_loc = function
    (* Expand one index of a slice if necessary. *)
      Abs(e) -> let (new_e, new_stmts) = expand_expr index_loc e in
      (Abs(new_e), new_stmts)
    | DimensionStart -> (DimensionStart, [])
    | DimensionEnd -> (DimensionEnd, [])
    | Rel(_) -> raise (IllegalExpression("relative - this shouldn't be possible")) in

  let expand_slice slice_loc = function
    (* Expand one or both sides as necessary. *)
      None -> (entire_dimension, [])
    | Some (Some (Abs(e)), None) ->
      let (start_e, start_stmts) = expand_expr (slice_loc ^ "_start") e in
      ((Some (Abs(start_e)), None), start_stmts)
    | Some (Some idx_start, Some idx_end) ->
      let (new_start, new_start_exprs) = expand_index (slice_loc ^ "_start") idx_start in
      let (new_end, new_end_exprs) = expand_index (slice_loc ^ "_end") idx_end in
      ((Some new_start, Some new_end), new_start_exprs @ new_end_exprs)
    | Some (Some _, None) | Some (None, _) -> raise (IllegalExpression("Illegal slice - this shouldn't be possible")) in

  let expand_assign asgn_loc (var_name, (row_slice, col_slice), formula) =
    (* expand_assign: Take an Assign and return a list of more
       atomic statements, with new variables replacing any
       complex expressions in the selection slices and with single
       index values desugared to expr:expr+1. *)
    try
      let (new_row_slice, row_exprs) = expand_slice (asgn_loc ^ "_" ^ var_name ^ "_row") row_slice in
      let (new_col_slice, col_exprs) = expand_slice (asgn_loc ^ "_" ^ var_name ^ "_col") col_slice in
      Assign(var_name, (Some new_row_slice, Some new_col_slice), formula) :: (row_exprs @ col_exprs)
    with IllegalExpression(s) ->
      raise (IllegalExpression("Illegal expression (" ^ s ^ ") in " ^
                               string_of_assign (var_name, (row_slice, col_slice), formula))) in

  let expand_init (r, c) (v, e) =
    Varinit((Some r, Some c), [(v, None)]) ::
    match e with
      None -> []
    | Some e -> [Assign (v, entire_range, Some e)] in

  let expand_dimension dim_loc = function
      None -> expand_expr dim_loc (LitInt(1))
    | Some e -> expand_expr dim_loc e in

  let expand_varinit fname ((row_dim, col_dim), inits) =
    (* expand_varinit: Take a Varinit and return a list of more atomic
       statements. Each dimension will be given a temporary ID, which
       will be declared as [1,1] _tmpXXX; the formula for tmpXXX will be
       set as a separate assignment; the original variable will be
       declared as [_tmpXXX, _tmpYYY] var; and the formula assignment
       will be applied to [:,:]. *)
    try
      let (row_e, row_stmts) = expand_dimension (fname ^ "_" ^ (String.concat "_" (List.map fst inits)) ^ "_row_dim") row_dim in
      let (col_e, col_stmts) = expand_dimension (fname ^ "_" ^ (String.concat "_" (List.map fst inits)) ^ "_col_dim") col_dim in
      row_stmts @ col_stmts @ List.concat (List.map (expand_init (row_e, col_e)) inits)
    with IllegalExpression(s) ->
      raise (IllegalExpression("Illegal expression (" ^ s ^ ") in " ^
                               string_of_varinit ((row_dim, col_dim), inits))) in

  let expand_stmt fname = function
    Assign(a) -> expand_assign fname a
  | Varinit(d, inits) -> expand_varinit fname (d, inits) in

  let expand_stmt_list fname stmts = List.concat (List.map (expand_stmt fname) stmts) in

  let expand_params fname params =
    let needs_sizevar = function
        ((None, None), _) -> false
      | _ -> true in
    let params_with_sizevar = List.map (fun x -> (idgen (fname ^ "_" ^ (snd x) ^ "_size"), x)) (List.filter needs_sizevar params) in
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
    let (new_sizevars, assertions, size_inits) = expand_params f.name f.params in
    let new_retval_id = idgen (f.name ^ "_retval") in
    let new_retval = Id(new_retval_id) in
    let retval_inits = [Varinit (one_by_one, [(new_retval_id, None)]);
                        Assign (new_retval_id, zero_comma_zero, Some (snd f.ret_val))] in
    let new_assert_id = idgen (f.name ^ "_assert") in
    let add_assert al a = BinOp(al, LogAnd, a) in
    let new_assert_expr = List.fold_left add_assert (LitInt(1)) assertions in
    let new_assert = Id(new_assert_id) in
    let assert_inits = [Varinit (one_by_one, [(new_assert_id, None)]);
                        Assign (new_assert_id, zero_comma_zero, Some new_assert_expr)] in
    {
      name = f.name;
      params = f.params;
      raw_asserts = [new_assert];
      body = new_sizevars @ size_inits @ retval_inits @ assert_inits @ expand_stmt_list f.name f.body;
      ret_val = (fst f.ret_val, new_retval)
    } in
  (imports, expand_stmt_list "global" globals, List.map expand_function functions, externs);;

let create_maps (imports, globals, functions, externs) =
  let vd_of_vi = function
    (*  vd_of_vi--- Take a bare Varinit from the previous transformations
        and return a (string, variable) pair    *)
      Varinit((Some r, Some c), [(v, None)]) -> (v, {
        var_rows = (match r with
              LitInt(1) -> DimOneByOne
            | Id(s) -> DimId(s)
            | _ -> raise (LogicError("Unrecognized expression for rows of " ^ v)));
        var_cols = (match c with
              LitInt(1) -> DimOneByOne
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

let single_formula e = {
  formula_row_start = DimensionStart;
  formula_row_end = Some DimensionEnd;
  formula_col_start = DimensionStart;
  formula_col_end = Some DimensionEnd;
  formula_expr = e;
}

let ternarize_exprs (globals, functions, externs) =
  let rec ternarize_expr lhs_var = function
      BinOp(e1, LogAnd, e2) ->
      let (new_e1, new_e1_vars) = ternarize_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = ternarize_expr lhs_var e2 in
      (Ternary(UnOp(Truthy,new_e1), UnOp(Truthy,new_e2), LitInt(0)), new_e1_vars @ new_e2_vars)
    | BinOp(e1, LogOr, e2) ->
      let (new_e1, new_e1_vars) = ternarize_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = ternarize_expr lhs_var e2 in
      (Ternary(UnOp(Truthy,new_e1), LitInt(1), UnOp(Truthy,new_e2)), new_e1_vars @ new_e2_vars)
    | BinOp(e1, op, e2) ->
      let (new_e1, new_e1_vars) = ternarize_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = ternarize_expr lhs_var  e2 in
      (BinOp(new_e1, op, new_e2), new_e1_vars @ new_e2_vars)
    | UnOp(op, e) ->
      let (new_e, new_e_vars) = ternarize_expr lhs_var e in
      (UnOp(op, new_e), new_e_vars)
    | Ternary(cond, e1, e2) ->
      let (new_cond, new_cond_vars) = ternarize_expr lhs_var cond in
      let (new_e1, new_e1_vars) = ternarize_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = ternarize_expr lhs_var e2 in
      (Ternary(new_cond, new_e1, new_e2), new_cond_vars @ new_e1_vars @ new_e2_vars)
    | Call(fname, args) ->
      let new_args_and_vars = List.map (ternarize_expr lhs_var) args in
      (Call(fname, (List.map fst new_args_and_vars)), List.concat (List.map snd new_args_and_vars))
    | Selection(e, (sl1, sl2)) ->
      let (new_e, new_e_vars) = ternarize_expr lhs_var e in
      let (new_sl1, new_sl1_vars) = ternarize_slice lhs_var sl1 in
      let (new_sl2, new_sl2_vars) = ternarize_slice lhs_var sl2 in
      (Selection(new_e, (new_sl1, new_sl2)), new_e_vars @ new_sl1_vars @ new_sl2_vars)
    | Precedence(e1, e2) ->
      let (new_e1, new_e1_vars) = ternarize_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = ternarize_expr lhs_var e2 in
      (Precedence(new_e1, new_e2), new_e1_vars @ new_e2_vars)
    | Switch(cond, cases, dflt) ->
      ternarize_switch lhs_var cases dflt cond
    (* | Debug(e) ->
      let (new_e, new_e_vars) = ternarize_expr lhs_var e in
      (Debug(new_e), new_e_vars) *)
    | e -> (e, [])
  and ternarize_switch lhs_var cases dflt cond =
    let (new_cond_expr, new_cond_vars) = (match cond with
          Some cond_expr ->
          let (lhs_varname, lhs_vardef) = lhs_var in
          let new_id = idgen (lhs_varname ^ "_switch_cond") in
          let (new_e, new_e_vars) = ternarize_expr lhs_var cond_expr in
          (Some (Selection(Id(new_id),(Some(Some(Rel(LitInt(0))),None),Some(Some(Rel(LitInt(0))),None)))),
           (new_id, {lhs_vardef with var_formulas = [single_formula new_e]}) ::
           new_e_vars)
        | None ->
          (None,[])
    ) in
    let new_cases_and_vars = List.map (ternarize_case lhs_var new_cond_expr) cases in
    let new_cases = List.map fst new_cases_and_vars in
    let new_case_vars = List.concat (List.map snd new_cases_and_vars) in
    let (new_dflt, new_dflt_vars) = ternarize_expr lhs_var dflt in
    let rec combine_everything = function
        [] -> new_dflt
      | (combined_cases, e) :: more_cases -> Ternary(combined_cases, e, combine_everything more_cases) in
    (combine_everything new_cases, new_cond_vars @ new_case_vars @ new_dflt_vars)
  and ternarize_case lhs_var cond (conds, e) =
    let new_conds_and_vars = List.map (ternarize_expr lhs_var) conds in
    let new_conds = List.map fst new_conds_and_vars in
    let new_cond_vars = List.concat (List.map snd new_conds_and_vars) in
    let (new_e, new_e_vars) = ternarize_expr lhs_var e in
    let unify_case_cond_and_switch_cond case_cond = function
        None -> case_cond
      | Some switch_cond -> BinOp(switch_cond,Eq,case_cond) in
    let rec unify_switch_cond_and_case_conds switch_cond = function
        [case_cond] -> unify_case_cond_and_switch_cond case_cond switch_cond
      | case_cond :: case_conds ->
        let (combined_expr, _) = ternarize_expr lhs_var
            (BinOp(unify_case_cond_and_switch_cond case_cond switch_cond, LogOr, unify_switch_cond_and_case_conds switch_cond case_conds)) in
        combined_expr
      | [] -> raise(LogicError("Empty case condition list")) in
    ((unify_switch_cond_and_case_conds cond new_conds, new_e),new_cond_vars @ new_e_vars)
  and ternarize_slice lhs_var = function
      None -> (None, [])
    | Some (i1, i2) ->
      let (new_i1, new_i1_vars) = ternarize_index lhs_var i1 in
      let (new_i2, new_i2_vars) = ternarize_index lhs_var i2 in
      (Some (new_i1, new_i2), new_i1_vars @ new_i2_vars)
  and ternarize_index lhs_var = function
      Some Abs(e) ->
      let (new_e, new_e_vars) = ternarize_expr lhs_var e in
      (Some(Abs(new_e)), new_e_vars)
    | Some Rel(e) ->
      let (new_e, new_e_vars) = ternarize_expr lhs_var e in
      (Some(Rel(new_e)), new_e_vars)
    | i -> (i, []) in
  let ternarize_formula lhs_var f =
    let (new_expr, new_vars) = ternarize_expr lhs_var f.formula_expr in
    ({f with formula_expr = new_expr}, new_vars) in
  let ternarize_variable varname vardef =
    let new_formulas_and_vars = List.map (ternarize_formula (varname, vardef)) vardef.var_formulas in
    ({vardef with var_formulas = List.map fst new_formulas_and_vars}, List.concat (List.map snd new_formulas_and_vars)) in
  let ternarize_variables fn_name m =
    let new_variables_and_maps = StringMap.mapi (fun varname vardef -> ternarize_variable (fn_name ^ "_" ^ varname) vardef) m in
    let add_item var_name (orig_var, new_vars) l = ((var_name, orig_var) :: fst l, new_vars :: snd l) in
    let combined_list = StringMap.fold add_item new_variables_and_maps ([],[]) in
    map_of_list (List.rev (fst combined_list) @ List.concat (snd combined_list)) in
  let ternarize_function fn_name fn_def = {fn_def with func_body = ternarize_variables fn_name fn_def.func_body} in
  (ternarize_variables "global" globals, StringMap.mapi ternarize_function functions, externs)

let reduce_ternaries (globals, functions, externs) =
  let rec reduce_expr lhs_var = function
    | BinOp(e1, op, e2) ->
      let (new_e1, new_e1_vars) = reduce_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = reduce_expr lhs_var  e2 in
      (BinOp(new_e1, op, new_e2), new_e1_vars @ new_e2_vars)
    | UnOp(op, e) ->
      let (new_e, new_e_vars) = reduce_expr lhs_var e in
      (UnOp(op, new_e), new_e_vars)
    | Ternary(cond, e1, e2) -> reduce_ternary lhs_var cond e1 e2
    | Call(fname, args) ->
      let new_args_and_vars = List.map (reduce_expr lhs_var) args in
      (Call(fname, (List.map fst new_args_and_vars)), List.concat (List.map snd new_args_and_vars))
    | Selection(e, (sl1, sl2)) ->
      let (new_e, new_e_vars) = reduce_expr lhs_var e in
      let (new_sl1, new_sl1_vars) = reduce_slice lhs_var sl1 in
      let (new_sl2, new_sl2_vars) = reduce_slice lhs_var sl2 in
      (Selection(new_e, (new_sl1, new_sl2)), new_e_vars @ new_sl1_vars @ new_sl2_vars)
    | Precedence(e1, e2) ->
      let (new_e1, new_e1_vars) = reduce_expr lhs_var e1 in
      let (new_e2, new_e2_vars) = reduce_expr lhs_var e2 in
      (Precedence(new_e1, new_e2), new_e1_vars @ new_e2_vars)
    (* | Debug(e) ->
      let (new_e, new_e_vars) = reduce_expr lhs_var e in
      (Debug(new_e), new_e_vars) *)
    | e -> (e, [])
  and reduce_ternary lhs_var cond e1 e2 =
    let (new_cond, new_cond_vars) = reduce_expr lhs_var cond in
    let (new_true_e, new_true_vars) = reduce_expr lhs_var e1 in
    let (new_false_e, new_false_vars) = reduce_expr lhs_var e2 in
    let (lhs_varname, lhs_vardef) = lhs_var in
    let new_cond_id = idgen (lhs_varname ^ "_truthiness") in
    let new_true_id = idgen (lhs_varname ^ "_values_if_true") in
    let new_false_id = idgen (lhs_varname ^ "_values_if_false") in
    (ReducedTernary(new_cond_id, new_true_id, new_false_id),
     (new_cond_id, {lhs_vardef with var_formulas = [single_formula (UnOp(Truthy,new_cond))]}) ::
     (new_true_id, {lhs_vardef with var_formulas = [single_formula new_true_e]}) ::
     (new_false_id, {lhs_vardef with var_formulas = [single_formula new_false_e]}) ::
     (new_cond_vars @ new_true_vars @ new_false_vars))
  and reduce_slice lhs_var = function
      None -> (None, [])
    | Some (i1, i2) ->
      let (new_i1, new_i1_vars) = reduce_index lhs_var i1 in
      let (new_i2, new_i2_vars) = reduce_index lhs_var i2 in
      (Some (new_i1, new_i2), new_i1_vars @ new_i2_vars)
  and reduce_index lhs_var = function
      Some Abs(e) ->
      let (new_e, new_e_vars) = reduce_expr lhs_var e in
      (Some(Abs(new_e)), new_e_vars)
    | Some Rel(e) ->
      let (new_e, new_e_vars) = reduce_expr lhs_var e in
      (Some(Rel(new_e)), new_e_vars)
    | i -> (i, []) in
  let reduce_formula lhs_var f =
    let (new_expr, new_vars) = reduce_expr lhs_var f.formula_expr in
    ({f with formula_expr = new_expr}, new_vars) in
  let reduce_variable varname vardef =
    let new_formulas_and_vars = List.map (reduce_formula (varname, vardef)) vardef.var_formulas in
    ({vardef with var_formulas = List.map fst new_formulas_and_vars}, List.concat (List.map snd new_formulas_and_vars)) in
  let reduce_variables fn_name m =
    let new_variables_and_maps = StringMap.mapi (fun varname vardef -> reduce_variable (fn_name ^ "_" ^ varname) vardef) m in
    let add_item var_name (orig_var, new_vars) l = ((var_name, orig_var) :: fst l, new_vars :: snd l) in
    let combined_list = StringMap.fold add_item new_variables_and_maps ([],[]) in
    map_of_list (List.rev (fst combined_list) @ List.concat (snd combined_list)) in
  let reduce_function fn_name fn_def = {fn_def with func_body = reduce_variables fn_name fn_def.func_body} in
  (reduce_variables "global" globals, StringMap.mapi reduce_function functions, externs)

let create_ast filename =
  let ast_imp_res = expand_file true filename in
  let ast_expanded = expand_expressions ast_imp_res in
  let ast_mapped = create_maps ast_expanded in check_semantics ast_mapped ;
  let ast_ternarized = ternarize_exprs ast_mapped in
  let ast_reduced = reduce_ternaries ast_ternarized in check_semantics ast_reduced ;
  ast_reduced
