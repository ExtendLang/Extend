
(* Extend code generator *)

open Ast
open CodeGenTypes
exception NotImplemented

let helper_functions = Hashtbl.create 10
let runtime_functions = Hashtbl.create 10

let index_map m =
  let add_item key _ (accum_map, accum_idx) = (StringMap.add key accum_idx accum_map, accum_idx + 1) in
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
  add_runtime_func "getSize" bt.value_p [|bt.var_instance_p;|] ;
  add_runtime_func "get_variable" bt.var_instance_p [|bt.extend_scope_p; bt.int_t|] ;
  add_runtime_func "null_init" (Llvm.void_type ctx) [|bt.extend_scope_p|] ;
  ()

let create_helper_functions ctx bt the_module =
  let create_def_bod fname rtype argtypes =
    let fn_def = Llvm.define_function fname (Llvm.function_type rtype (Array.of_list argtypes)) the_module in
    let fn_bod = Llvm.builder_at_end ctx (Llvm.entry_block fn_def) in
    (fn_def, fn_bod) in

  let create_is_subrange_1x1 fname =
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

  let create_box_native_string_list fname =
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
    Hashtbl.add helper_functions fname fn_def in

  let create_box_value_string fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.value_p [bt.string_p] in
    let str = Llvm.param fn_def 0 in
    let ret_val = Llvm.build_malloc bt.value_t "" fn_bod in
    let sp = Llvm.build_struct_gep ret_val (value_field_index String) "str_pointer" fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.char_t (value_field_flags_index String)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" fn_bod) fn_bod in
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  let create_box_value_float fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.value_p [bt.float_t] in
    let str = Llvm.param fn_def 0 in
    let ret_val = Llvm.build_malloc bt.value_t "" fn_bod in
    let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" fn_bod) fn_bod in
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in


  let create_box_single_value fname =
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
    Hashtbl.add helper_functions fname fn_def in

    create_is_subrange_1x1 "is_subrange_1x1";
    (*create_get_val "get_val";
    create_deref_subrange "deref_subrange";*)
    create_new_string "new_string";
    create_box_native_string_list "box_native_string_list";
    create_box_value_string "box_value_string";
    create_box_single_value "box_single_value";
    create_box_value_float "box_value_float";
    ()
let create_main entry_point ctx bt the_module =
  let main_def = Llvm.define_function "main"
      (Llvm.function_type bt.int_t (Array.of_list [bt.int_t; bt.char_p_p]))
      the_module in
  let main_bod = Llvm.builder_at_end ctx (Llvm.entry_block main_def) in
  let inp = Llvm.build_alloca bt.subrange_t "input_arg" main_bod in
  (* Put input args in inp *)
  let _ = Llvm.build_call entry_point (Array.of_list [inp]) "" main_bod in
  let str_format_str = Llvm.build_global_stringptr "%s\n" "fmt" main_bod in
  let int_format_str = Llvm.build_global_stringptr "%d\n" "fmt" main_bod in
  let boxed_args = Llvm.build_call (Hashtbl.find helper_functions "box_native_string_list") [|(Llvm.param main_def 0);(Llvm.param main_def 1)|] "args" main_bod in
  let _ = Llvm.build_ret (Llvm.const_int bt.int_t 0) main_bod in
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
    let llvm_ftype = Llvm.function_type base_types.value_p (Array.of_list (List.map (fun a -> base_types.subrange_p) func.extern_fn_params)) in
    let llvm_fn = Llvm.declare_function fname llvm_ftype base_module in
    StringMap.add fname llvm_fn accum_map in
  let library_functions = StringMap.fold declare_library_function externs StringMap.empty in
  let define_user_function fname func =
    let llvm_fname = "extend_" ^ fname in
    let llvm_ftype = Llvm.function_type base_types.value_p (Array.of_list (List.map (fun a -> base_types.subrange_p) func.func_params)) in
    let llvm_fn = Llvm.define_function llvm_fname llvm_ftype base_module in
    (func, llvm_fn) in
  let extend_functions = StringMap.mapi define_user_function functions in
  let function_llvalues = StringMap.fold (fun k a b -> StringMap.add k a b) (StringMap.map (fun (b, c) -> c) extend_functions) library_functions in

  let getVal = Hashtbl.find runtime_functions "getVal" in (*getVal retrieves the value of a variable instance for a specific x and y*)
  let sizeof = Hashtbl.find runtime_functions "getSize" in (*getSize does not work yet*)
  let getVar = Hashtbl.find runtime_functions "get_variable" in (*getVar retrieves a variable instance based on the offset. It instanciates the variable if it does not exist yet*)
  let nullAll = Hashtbl.find runtime_functions "null_init" in (*Call to nullAll nulls the scope variable instances, since they are not null initialized*)
  (*build_expr simply builds naive LLVM expressions.*)
  let rec build_expr expr builder mapping scope = match expr with
      LitInt(i) -> let vvv = Llvm.const_float base_types.float_t (float_of_int i) in
      let ret_val = Llvm.build_malloc base_types.value_t "" builder in
      let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" builder in
      let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" builder) builder in
      let _ = Llvm.build_store vvv sp builder in
      ret_val
    | Id(name) -> (try (Llvm.build_call getVal [|(Llvm.build_call getVar [|scope; Llvm.const_int base_types.int_t (StringMap.find name mapping)|] "" builder); Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "" builder) with Not_found -> Llvm.build_malloc base_types.value_t "" builder) (*TODO*)
    | Selection(expr, sel) -> build_expr expr builder mapping scope
    | Precedence(a,b) -> ignore (build_expr a builder mapping scope); build_expr b builder mapping scope
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
               fun a b -> (
                   Llvm.build_call
                     (Hashtbl.find helper_functions "box_single_value")
                     (Array.of_list [(build_expr b builder mapping scope)])
                     ""
                     builder
                 ) :: a) [] exl)) in
      Llvm.build_call (
        StringMap.find fn function_llvalues
      ) args "" builder
    | UnOp(op,expr) -> (match op with
          SizeOf -> print_endline (string_of_expr expr); raise NotImplemented
        | _ -> print_endline (string_of_expr expr);raise NotImplemented)
    | _ -> print_endline (string_of_expr expr);raise NotImplemented in


  (*getDefn simply looks up the correct definition for a dimension declaration of a variable. Note that currently it is ambiguous whether it is a variable or a literal. TOOD: consider negative numbers*)
  let getDefn x sm = match x with DimId(a) -> StringMap.find a sm | DimInt(a) -> a
  (*buildDimSide builds one end (e.g. row start, row end, col start, ...) of a formula definition, TODO: remove literals for (not atstart)*)
  and buildDimSide index boolAll intDim builder atstart ids = (*print_endline (string_of_index index);*) (match index with
        None -> Llvm.build_store (Llvm.const_int base_types.bool_t 1) boolAll builder
      | Some(Abs(e)) -> (
          ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
          Llvm.build_store (match e with LitInt(i) -> Llvm.const_int base_types.int_t i) intDim builder
        )
      | Some(Rel(e)) -> (
          ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
          Llvm.build_store (match e with LitInt(i) -> Llvm.const_int base_types.int_t i) intDim builder
        )
      | _ -> if (atstart) then (
          ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
          Llvm.build_store (Llvm.const_int base_types.int_t 0) intDim builder
        ) else (
          ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
          Llvm.build_store (Llvm.const_int base_types.int_t 1) intDim builder
        )
    ) in

  (*build formula creates a formula declaration in a separate method from the function it belongs to*)
  let build_formula storage_addr element scopeMapping builder =
    buildDimSide (Some element.formula_col_start) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstCols) "" builder) (Llvm.build_struct_gep storage_addr (formula_field_index ColStartNum) "" builder) builder true scopeMapping;
    buildDimSide (Some element.formula_row_start) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstRow) "" builder) (Llvm.build_struct_gep storage_addr (formula_field_index RowStartNum) "" builder) builder true scopeMapping;
    buildDimSide element.formula_col_end (Llvm.build_struct_gep storage_addr (formula_field_index ToLastCol) "" builder) (Llvm.build_struct_gep storage_addr (formula_field_index ColEndNum) "" builder) builder false scopeMapping;
    buildDimSide element.formula_row_end (Llvm.build_struct_gep storage_addr (formula_field_index ToLastRow) "" builder) (Llvm.build_struct_gep storage_addr (formula_field_index RowEndNum) "" builder) builder false scopeMapping;
    let form_decl = Llvm.define_function "" base_types.formula_call_t base_module in
    let nbuilder = Llvm.builder_at_end context (Llvm.entry_block form_decl)
    and _ = Llvm.build_store form_decl (Llvm.build_struct_gep storage_addr (formula_field_index FormulaCall) "" builder) builder in
    Llvm.build_ret (build_expr element.formula_expr nbuilder scopeMapping (Llvm.param form_decl 0)) nbuilder; () in
  let build_function key (desc, func) =
    let builder = Llvm.builder_at_end context (Llvm.entry_block func)
    and cardinal = StringMap.cardinal desc.func_body in
    let var_defns = Llvm.build_array_malloc base_types.var_defn_t (Llvm.const_int base_types.int_t cardinal) "" builder
    and var_insts = Llvm.build_array_malloc base_types.var_instance_p (Llvm.const_int base_types.int_t cardinal) "" builder
    and scope_obj = Llvm.build_malloc base_types.extend_scope_t "" builder in
    (*Store variable definition and instance*)
    let _ = Llvm.build_store var_defns (Llvm.build_struct_gep scope_obj (scope_field_type_index VarDefn) "" builder) builder
    and _ = Llvm.build_store var_insts (Llvm.build_struct_gep scope_obj (scope_field_type_index VarInst) "" builder) builder
    and _ = Llvm.build_store (Llvm.const_int base_types.int_t cardinal) (Llvm.build_struct_gep scope_obj (scope_field_type_index VarNum) "" builder) builder in
    let _ = Llvm.build_call nullAll [|scope_obj|] "" builder in
    (*iterates over formulas defined*)
    let (scope, i) = StringMap.fold (fun ke va (sm, count) ->
        let defn = (Llvm.build_in_bounds_gep var_defns [|Llvm.const_int base_types.int_t count|] "" builder)
        and numForm = List.length va.var_formulas in
        let formulas = Llvm.build_array_malloc base_types.formula_t (Llvm.const_int base_types.int_t numForm) "" builder in
        let _ = (match va.var_rows with
              DimInt(a) -> Llvm.build_store (Llvm.const_int base_types.bool_t 1) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" builder) builder
            | DimId(a) -> (
                Llvm.build_store (Llvm.const_int base_types.bool_t 0) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" builder) builder;
                Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_rows sm)) (Llvm.build_struct_gep defn (var_defn_field_index Rows) "" builder) builder;
                Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_cols sm)) (Llvm.build_struct_gep defn (var_defn_field_index Cols) "" builder) builder
              )
          ) in
        let _ = Llvm.build_store (Llvm.const_int base_types.int_t numForm) (Llvm.build_struct_gep defn (var_defn_field_index NumFormulas) "" builder) builder
        and _ = Llvm.build_store formulas (Llvm.build_struct_gep defn (var_defn_field_index Formulas) "" builder) builder
        and _ = Llvm.build_store (Llvm.build_global_stringptr ke "" builder) (Llvm.build_struct_gep defn 5 "" builder) builder in
        let _  = List.fold_left (fun st elem -> build_formula st elem sm builder; Llvm.build_in_bounds_gep st [|Llvm.const_int base_types.int_t 1|] "" builder) formulas va.var_formulas
        in (StringMap.add ke count sm, count + 1)
        (*List.fold_left (fun s v -> v :: s) st va.var_formulas*)
      ) desc.func_body (StringMap.empty, 0) in
    let (dim, ret) = desc.func_ret_val in
    match ret with
      Id(name) -> ignore (Llvm.build_ret (Llvm.build_call getVal [|(Llvm.build_call getVar [|scope_obj; Llvm.const_int base_types.int_t (StringMap.find name scope)|] "" builder); Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "" builder) builder)
    | _ -> print_endline (string_of_expr ret);raise NotImplemented in

  (*iterates over function definitions*)
  StringMap.iter build_function extend_functions ;

  (* Define the LLVM entry point for the program *)
  let entry_point = StringMap.find "main" function_llvalues in
  create_main entry_point context base_types base_module ;

  base_module

let build_this ast_mapped =
  let modu = (translate ast_mapped) in
  let res = Llvm_analysis.assert_valid_module modu in
  modu
