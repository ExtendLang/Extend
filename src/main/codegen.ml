(* Extend code generator *)

open Ast
open CodeGenTypes
exception NotImplemented
exception LogicError of string

type symbol = LocalVariable of int | GlobalVariable of int | FunctionParameter of int | ExtendFunction of int
and  symbolTable = symbol StringMap.t
and  symbolTableType = Locals | Globals | ExtendFunctions

let helper_functions = Hashtbl.create 10
let runtime_functions = Hashtbl.create 10

let index_map table_type m =
  let add_item key _ (accum_map, accum_idx) =
    let index_val = match table_type with Locals -> LocalVariable(accum_idx) | Globals -> GlobalVariable(accum_idx) | ExtendFunctions -> ExtendFunction(accum_idx) in
    (StringMap.add key index_val accum_map, accum_idx + 1) in
  StringMap.fold add_item m (StringMap.empty, 0)

let (=>) struct_ptr elem = (fun val_name builder ->
    let the_pointer = Llvm.build_struct_gep struct_ptr elem "the_pointer" builder in
    Llvm.build_load the_pointer val_name builder);;

let create_runtime_functions ctx bt the_module =
  let add_runtime_func fname returntype arglist =
    let the_func = Llvm.declare_function fname (Llvm.function_type returntype arglist) the_module
    in Hashtbl.add runtime_functions fname the_func in
  add_runtime_func "strlen" bt.long_t [|bt.char_p|];
  add_runtime_func "llvm.memcpy.p0i8.p0i8.i64" bt.void_t [|bt.char_p; bt.char_p; bt.long_t; bt.int_t; bt.bool_t|] ;
  add_runtime_func "getVal" bt.value_p [|bt.var_instance_p; bt.int_t; bt.int_t|] ;
  add_runtime_func "deepCopy" bt.value_p [|bt.value_p;|] ;
  add_runtime_func "freeMe" (Llvm.void_type ctx) [|bt.extend_scope_p;|] ;
  add_runtime_func "getSize" bt.value_p [|bt.var_instance_p;|] ;
  add_runtime_func "get_variable" bt.var_instance_p [|bt.extend_scope_p; bt.int_t|] ;
  add_runtime_func "null_init" (Llvm.void_type ctx) [|bt.extend_scope_p|] ;
  ()

let create_helper_functions ctx bt the_module =
  let create_def_bod fname rtype argtypes =
    let fn_def = Llvm.define_function fname (Llvm.function_type rtype (Array.of_list argtypes)) the_module in
    let fn_bod = Llvm.builder_at_end ctx (Llvm.entry_block fn_def) in
    (fn_def, fn_bod) in

  (* let create_is_subrange_1x1 fname =
    let is_index_one fn builder idx =
      let the_value = ((Llvm.param fn 0) => (subrange_field_index idx)) "the_value" builder in
      let the_bool = Llvm.build_icmp Llvm.Icmp.Eq the_value (Llvm.const_int bt.int_t 1) "the_bool" builder in
      the_bool in
    let (fn_def, fn_bod) = create_def_bod fname bt.bool_t [bt.subrange_p] in
    let one_row = is_index_one fn_def fn_bod SubrangeRows in
    let one_col = is_index_one fn_def fn_bod SubrangeCols in
    let one_by_one = Llvm.build_and one_row one_col "one_by_one" fn_bod in
    let _ = Llvm.build_ret one_by_one fn_bod in
    Hashtbl.add helper_functions fname fn_def in
 *)
  let create_new_string fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.string_p [bt.char_p] in
    let the_string_ptr = Llvm.build_malloc bt.string_t "the_string_ptr" fn_bod in
    let src_char_ptr = Llvm.param fn_def 0 in
    let dst_char_ptr_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringCharPtr) "dst_char_ptr_ptr" fn_bod in
    let string_len = Llvm.build_call (Hashtbl.find runtime_functions "strlen") [|src_char_ptr|] "string_len" fn_bod in
    let extra_byte = Llvm.build_add string_len (Llvm.const_int bt.long_t 1) "extra_byte" fn_bod in
    let strlen_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringLen) "strlen_ptr" fn_bod in
    let refcount_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringRefCount) "refcount" fn_bod in
    let dst_char_ptr = Llvm.build_array_malloc bt.char_t extra_byte "dst_char_ptr" fn_bod in
    let _ = Llvm.build_store dst_char_ptr dst_char_ptr_ptr fn_bod in
    let _ = Llvm.build_call (Hashtbl.find runtime_functions "llvm.memcpy.p0i8.p0i8.i64")
        [| dst_char_ptr ; src_char_ptr ; extra_byte ; (Llvm.const_int bt.int_t 0) ; (Llvm.const_int bt.bool_t 0) |]
        "" fn_bod in
    let _ = Llvm.build_store string_len strlen_ptr fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) refcount_ptr fn_bod in
    let _ = Llvm.build_ret the_string_ptr fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  (* let create_box_native_string_list fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.string_p_p [bt.int_t; bt.char_p_p] in
    let argc = Llvm.param fn_def 0 in
    let argv = Llvm.param fn_def 1 in
    let ret_val = Llvm.build_array_malloc bt.string_p argc "ret_val" fn_bod in
    let i_ptr = Llvm.build_alloca bt.int_t "i_ptr" fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) i_ptr fn_bod in (* i = 0; *)
    let pred_bb = Llvm.append_block ctx "while_pred" fn_def in
    let body_bb = Llvm.append_block ctx "while_body" fn_def in
    let merge_bb = Llvm.append_block ctx "merge" fn_def in
    let _ = Llvm.build_br pred_bb fn_bod in
    let pred_builder = Llvm.builder_at_end ctx pred_bb in
    let i_val = Llvm.build_load i_ptr "i" pred_builder in
    let pred_bool = Llvm.build_icmp Llvm.Icmp.Slt i_val argc "i_lt_argc" pred_builder in (* i < argc *)
    let _ = Llvm.build_cond_br pred_bool body_bb merge_bb pred_builder in
    let body_builder = Llvm.builder_at_end ctx body_bb in
    let i_val = Llvm.build_load i_ptr "i" body_builder in
    let argv_i_addr = Llvm.build_in_bounds_gep argv [|i_val|] "argv_i_addr" body_builder in
    let argv_i = Llvm.build_load argv_i_addr "argv_i" body_builder in
    let ns_ptr = Llvm.build_call (Hashtbl.find helper_functions "new_string") [|argv_i|] "ns_ptr" body_builder in
    let dst = Llvm.build_in_bounds_gep ret_val [|i_val|] "dst" body_builder in
    let _ = Llvm.build_store ns_ptr dst body_builder in
    let i_plus_1 = Llvm.build_add i_val (Llvm.const_int bt.int_t 1) "i_plus_1" body_builder in
    let _ = Llvm.build_store i_plus_1 i_ptr body_builder in
    let _ = Llvm.build_br pred_bb body_builder in
    let merge_builder = Llvm.builder_at_end ctx merge_bb in
    let _ = Llvm.build_ret ret_val merge_builder in
    Hashtbl.add helper_functions fname fn_def in *)

  let create_box_value_string fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.value_p [bt.string_p] in
    let str = Llvm.param fn_def 0 in
    let ret_val = Llvm.build_malloc bt.value_t "" fn_bod in
    let sp = Llvm.build_struct_gep ret_val (value_field_index String) "str_pointer" fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.char_t (value_field_flags_index String)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" fn_bod) fn_bod in
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  (* let create_box_value_float fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.value_p [bt.float_t] in
    let str = Llvm.param fn_def 0 in
    let ret_val = Llvm.build_malloc bt.value_t "" fn_bod in
    let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" fn_bod) fn_bod in
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in *)


  (* let create_box_single_value fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.subrange_p [bt.value_p] in
    let value = Llvm.param fn_def 0 in
    let subrange = Llvm.build_malloc bt.subrange_t "" fn_bod in
    let var_instance = Llvm.build_malloc bt.var_instance_t "" fn_bod in
    let rp = Llvm.build_struct_gep subrange (subrange_field_index BaseRangePtr) "range_p" fn_bod in
    let vp = Llvm.build_struct_gep var_instance (var_instance_field_index Values) "value_p" fn_bod in
    let _ = Llvm.build_store value vp fn_bod in
    let _ = Llvm.build_store var_instance rp fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetCol) "" fn_bod) fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetRow) "" fn_bod) fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeRows) "" fn_bod) fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeCols) "" fn_bod) fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep var_instance (var_instance_field_index Rows) "" fn_bod) fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep var_instance (var_instance_field_index Cols) "" fn_bod) fn_bod in
    let _ = Llvm.build_ret subrange fn_bod in
    Hashtbl.add helper_functions fname fn_def in *)

    (* create_is_subrange_1x1 "is_subrange_1x1"; *)
    (*create_get_val "get_val";
    create_deref_subrange "deref_subrange";*)
    create_new_string "new_string";
    (* create_box_native_string_list "box_native_string_list"; *)
    create_box_value_string "box_value_string";
    (* create_box_single_value "box_single_value"; *)
    (* create_box_value_float "box_value_float";  *)
    ()

let translate (globals, functions, externs) =

  (* LLVM Boilerplate *)
  let context = Llvm.global_context () in
  let base_module = Llvm.create_module context "Extend" in
  let base_types = setup_types context in

  (* Declare the runtime functions that we need to call *)
  create_runtime_functions context base_types base_module ;
  create_helper_functions context base_types base_module ;

  (* Build function_llvalues, which is a StringMap from function name to llvalue.
   * It includes both functions from external libraries, such as the standard library,
   * and functions declared within Extend. *)
  let declare_library_function fname func accum_map =
    let llvm_ftype = Llvm.function_type base_types.value_p (Array.of_list (List.map (fun a -> base_types.value_p) func.extern_fn_params)) in
    let llvm_fn = Llvm.declare_function fname llvm_ftype base_module in
    StringMap.add fname llvm_fn accum_map in
  let library_functions = StringMap.fold declare_library_function externs StringMap.empty in
  let define_user_function fname func =
    let llvm_fname = "extend_" ^ fname in
    let llvm_ftype = Llvm.function_type base_types.value_p (Array.of_list (List.map (fun a -> base_types.value_p) func.func_params)) in
    let llvm_fn = Llvm.define_function llvm_fname llvm_ftype base_module in
    (func, llvm_fn) in
  let extend_functions = StringMap.mapi define_user_function functions in
  let function_llvalues = StringMap.fold StringMap.add (StringMap.map snd extend_functions) library_functions in

  (* Build the global symbol table *)
  let (global_symbols, num_globals) = index_map Globals globals in
  let (extend_fn_numbers, num_extend_fns) = index_map ExtendFunctions extend_functions in

  (* Create the global array that will hold each function's array of var_defns. *)
  let vardefn_ptr = Llvm.const_pointer_null base_types.var_defn_p in
  let vardefn_array = Array.make (StringMap.cardinal extend_functions) vardefn_ptr in
  let array_of_vardefn_ptrs = Llvm.define_global "global_vardefn_ptrs" (Llvm.const_array base_types.var_defn_p vardefn_array) base_module in

  let main_def = Llvm.define_function "main" (Llvm.function_type base_types.int_t [|base_types.int_t; base_types.char_p_p|]) base_module in
  let main_bod = Llvm.builder_at_end context (Llvm.entry_block main_def) in

  (* Look these two up once and for all *)
  let deepCopy = Hashtbl.find runtime_functions "deepCopy" in
  let freeMe = Hashtbl.find runtime_functions "freeMe" in
  let getVal = Hashtbl.find runtime_functions "getVal" in (*getVal retrieves the value of a variable instance for a specific x and y*)
  let getVar = Hashtbl.find runtime_functions "get_variable" in (*getVar retrieves a variable instance based on the offset. It instanciates the variable if it does not exist yet*)

  (* build_formula_function takes a symbol table and an expression, builds the LLVM function, and returns the llvalue of the function *)
  let build_formula_function (varname, formula_idx) symbols formula_expr =
    let form_decl = Llvm.define_function ("formula_fn_" ^ varname ^ "_num_" ^ (string_of_int formula_idx)) base_types.formula_call_t base_module in
    let builder = Llvm.builder_at_end context (Llvm.entry_block form_decl) in
    let local_scope = Llvm.param form_decl 0 in
    let rec build_expr exp = match exp with
        LitInt(i) -> let vvv = Llvm.const_float base_types.float_t (float_of_int i) in
        let ret_val = Llvm.build_alloca base_types.value_t "" builder in
        let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" builder in
        let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" builder) builder in
        let _ = Llvm.build_store vvv sp builder in
        ret_val
      | LitFlt(i) -> let vvv = Llvm.const_float base_types.float_t i in
        let ret_val = Llvm.build_alloca base_types.value_t "" builder in
        let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" builder in
        let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" builder) builder in
        let _ = Llvm.build_store vvv sp builder in
        ret_val
      | Id(name) ->
        (
          match (try StringMap.find name symbols with Not_found -> raise(LogicError("Something went wrong with your semantic analysis - " ^ name ^ " not found"))) with
            LocalVariable(i) ->
            let llvm_var = Llvm.build_call getVar [|local_scope; Llvm.const_int base_types.int_t i|] "" builder in
            Llvm.build_call getVal [|llvm_var; Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "" builder
          | GlobalVariable(i) -> raise(NotImplemented)
          | FunctionParameter(i) -> raise(NotImplemented)
          | ExtendFunction(i) -> raise(LogicError("Something went wrong with your semantic analyis - function " ^ name ^ " used as variable in RHS for " ^ varname))
        )
      | Selection(expr, sel) -> build_expr expr
      | Precedence(a,b) -> ignore (build_expr a); build_expr b
      | LitString(str) ->
        let boxxx = Llvm.build_call
            (Hashtbl.find helper_functions "new_string")
            (Array.of_list [
                Llvm.build_global_stringptr str "glob_str" builder
              ]) "boxed_str" builder in
        let boxx = Llvm.build_call
            (Hashtbl.find helper_functions "box_value_string")
            (Array.of_list [boxxx]) "box_value_str" builder
        in boxx
      | Call(fn,exl) -> (*TODO: Call needs to be reviewed. Possibly switch call arguments to value_p*)
        let args = Array.of_list
            (List.rev (List.fold_left (
                 fun a b -> (build_expr b) :: a) [] exl)) in
        let result = Llvm.build_call (
          StringMap.find fn function_llvalues
        ) args "" builder in
        result
      | UnOp(SizeOf,expr) -> let vvv = Llvm.const_float base_types.float_t 0.0 in
        let ret_val = Llvm.build_malloc base_types.value_t "" builder in
        let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" builder in
        let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" builder) builder in
        let _ = Llvm.build_store vvv sp builder in
        ret_val
      | UnOp( _, expr) -> print_endline (Ast.string_of_expr exp); raise NotImplemented
      | unknown_expr -> print_endline (string_of_expr unknown_expr);raise NotImplemented in
    let cpy = Llvm.build_call deepCopy [|(build_expr formula_expr)|] "" builder in
    let _ = Llvm.build_call freeMe [||] in
    let _ = Llvm.build_ret (cpy) builder in
    form_decl in

  (*build formula creates a formula declaration in a separate method from the function it belongs to*)
  let build_formula (varname, idx) formula_array element symbols =
    let storage_addr = Llvm.build_in_bounds_gep formula_array [|Llvm.const_int base_types.int_t idx|] "" main_bod in
    (*buildDimSide builds one end (e.g. row start, row end, col start, ...) of a formula definition, TODO: remove literals for (not atstart)*)
    let buildDimSide index boolAll intDim builder atstart = (*print_endline (string_of_index index);*) (match index with
          None -> Llvm.build_store (Llvm.const_int base_types.bool_t 1) boolAll builder
        | Some(Abs(e)) -> (
            ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
            Llvm.build_store (
              match e with LitInt(i) -> Llvm.const_int base_types.int_t i
              | _ -> raise NotImplemented
            ) intDim builder
          )
        | Some(Rel(e)) -> (
            ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
            Llvm.build_store (
              match e with LitInt(i) -> Llvm.const_int base_types.int_t i
              | _ -> raise NotImplemented
            ) intDim builder
          )
        | _ -> if (atstart) then (
            ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
            Llvm.build_store (Llvm.const_int base_types.int_t 0) intDim builder
          ) else (
            ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
            Llvm.build_store (Llvm.const_int base_types.int_t 1) intDim builder
          )
      ) in
    let _ = buildDimSide (Some element.formula_col_start) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstCols) "" main_bod) (Llvm.build_struct_gep storage_addr (formula_field_index ColStartNum) "" main_bod) main_bod true in ();
    let _ = buildDimSide (Some element.formula_row_start) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstRow) "" main_bod) (Llvm.build_struct_gep storage_addr (formula_field_index RowStartNum) "" main_bod) main_bod true in ();
    let _ = buildDimSide element.formula_col_end (Llvm.build_struct_gep storage_addr (formula_field_index ToLastCol) "" main_bod) (Llvm.build_struct_gep storage_addr (formula_field_index ColEndNum) "" main_bod) main_bod false in ();
    let _ = buildDimSide element.formula_row_end (Llvm.build_struct_gep storage_addr (formula_field_index ToLastRow) "" main_bod) (Llvm.build_struct_gep storage_addr (formula_field_index RowEndNum) "" main_bod) main_bod false in ();
    let form_decl = build_formula_function (varname, idx) symbols element.formula_expr in
    let _ = Llvm.build_store form_decl (Llvm.build_struct_gep storage_addr (formula_field_index FormulaCall) "" main_bod) main_bod in
    () in

  (* Builds a var_defn struct for each variable *)
  let build_var_defn defn varname va symbols =
    let numForm = List.length va.var_formulas in
    let formulas = Llvm.build_array_malloc base_types.formula_t (Llvm.const_int base_types.int_t numForm) "" main_bod in
    (*getDefn simply looks up the correct definition for a dimension declaration of a variable. Note that currently it is ambiguous whether it is a variable or a literal. TOOD: consider negative numbers*)
    let getDefn = function
        DimId(a) -> (match StringMap.find a symbols with LocalVariable(i) -> i | _ -> raise(NotImplemented))
      | DimInt(1) -> 1
      | DimInt(_) -> raise(NotImplemented) in
    let _ = (match va.var_rows with
          DimInt(1) -> Llvm.build_store (Llvm.const_int base_types.bool_t 1) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" main_bod) main_bod
        | DimInt(_) -> raise(NotImplemented)
        | DimId(a) -> (
            let _ = Llvm.build_store (Llvm.const_int base_types.bool_t 0) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" main_bod) main_bod in ();
            let _ = Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_rows)) (Llvm.build_struct_gep defn (var_defn_field_index Rows) "" main_bod) main_bod in ();
            Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_cols)) (Llvm.build_struct_gep defn (var_defn_field_index Cols) "" main_bod) main_bod
          )
      ) in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t numForm) (Llvm.build_struct_gep defn (var_defn_field_index NumFormulas) "" main_bod) main_bod
    and _ = Llvm.build_store formulas (Llvm.build_struct_gep defn (var_defn_field_index Formulas) "" main_bod) main_bod
    and _ = Llvm.build_store (Llvm.build_global_stringptr varname "" main_bod) (Llvm.build_struct_gep defn (var_defn_field_index VarName) "" main_bod) main_bod in
    List.iteri (fun idx elem -> build_formula (varname, idx) formulas elem symbols) va.var_formulas in

  let build_scope_obj fname symbols vars static_location_ptr var_defns_loc builder =
    let cardinal = Llvm.const_int base_types.int_t (StringMap.cardinal vars) in
    let build_var_defns =
      let static_var_defns = Llvm.build_array_malloc base_types.var_defn_t cardinal (fname ^ "static_var_defns") main_bod in
      let _ = Llvm.build_store static_var_defns static_location_ptr main_bod in
      let add_variable varname va (sm, count) =
        let fullname = fname ^ "_" ^ varname in
        let defn = (Llvm.build_in_bounds_gep static_var_defns [|Llvm.const_int base_types.int_t count|] (fullname ^ "_defn") main_bod) in
        let _ = build_var_defn defn fullname va symbols in
        (StringMap.add varname count sm, count + 1) in
      ignore (StringMap.fold add_variable vars (StringMap.empty, 0)) in

    let var_defns = Llvm.build_load var_defns_loc (fname ^ "_global_defn_ptr_loc") builder in
    let var_insts = Llvm.build_array_malloc base_types.var_instance_p cardinal "var_insts" builder in
    let scope_obj = Llvm.build_malloc base_types.extend_scope_t "scope_obj" builder in

    (*Store variable definition and instance*)
    let _ = Llvm.build_store var_defns (Llvm.build_struct_gep scope_obj (scope_field_type_index VarDefn) "" builder) builder in
    let _ = Llvm.build_store var_insts (Llvm.build_struct_gep scope_obj (scope_field_type_index VarInst) "" builder) builder in
    let _ = Llvm.build_store cardinal (Llvm.build_struct_gep scope_obj (scope_field_type_index VarNum) "" builder) builder in
    let _ = Llvm.build_call (Hashtbl.find runtime_functions "null_init") [|scope_obj|] "" builder in
    build_var_defns ; scope_obj in

  let build_function fname (fn_def, fn_llvalue) =
    (* Build the symbol table for this function *)
    let (local_indices, num_locals) = index_map Locals fn_def.func_body in
    let add_param (st, idx) param_name =
      let new_st = StringMap.add param_name (FunctionParameter(idx)) st in
      (new_st, idx + 1) in
    let (params_and_globals, _) = List.fold_left add_param (global_symbols, 0) (List.map snd fn_def.func_params) in
    let symbols = StringMap.fold StringMap.add local_indices params_and_globals in
    let fn_idx = match StringMap.find fname extend_fn_numbers with ExtendFunction(i) -> i | _ -> raise(LogicError(fname ^ " not in function table")) in
    let builder = Llvm.builder_at_end context (Llvm.entry_block fn_llvalue) in
    let static_location_ptr = Llvm.build_in_bounds_gep array_of_vardefn_ptrs [|Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t fn_idx|] (fname ^ "_global_defn_ptr") main_bod in
    let var_defns_loc = Llvm.build_in_bounds_gep array_of_vardefn_ptrs [|Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t fn_idx|] (fname ^ "_local_defn_ptr") builder in

    let scope_obj = build_scope_obj fname symbols fn_def.func_body static_location_ptr var_defns_loc builder in

    let ret = snd fn_def.func_ret_val in
    match ret with
      Id(name) ->
      (
        match (try StringMap.find name symbols with Not_found -> raise(LogicError("Something went wrong with your semantic analysis - " ^ name ^ " not found"))) with
          LocalVariable(i) ->
          let llvm_var = Llvm.build_call getVar [|scope_obj; Llvm.const_int base_types.int_t i|] "return_variable" builder in
          let llvm_retval = Llvm.build_call getVal [|llvm_var; Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "return_value" builder in
          ignore (Llvm.build_ret llvm_retval builder)
        | _ -> print_endline (string_of_expr ret); raise(TransformedAway("The return value should always have been transformed into a local variable"))
      )
    | _ -> print_endline (string_of_expr ret); raise(TransformedAway("The return value should always have been transformed into a local variable")) in
  (* End of build_function *)

  (*iterates over function definitions*)
  StringMap.iter build_function extend_functions ;

  (* Define the LLVM entry point for the program *)
  let extend_entry_point = StringMap.find "main" function_llvalues in
  let inp = Llvm.build_alloca base_types.value_t "input_arg" main_bod in
  let _ = Llvm.build_call extend_entry_point (Array.of_list [inp]) "" main_bod in
  let _ = Llvm.build_ret (Llvm.const_int base_types.int_t 0) main_bod in

  base_module

let build_this ast_mapped =
  let modu = (translate ast_mapped) in
  let _ = Llvm_analysis.assert_valid_module modu in
  modu
