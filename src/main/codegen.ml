(* Extend code generator *)

open Ast
open CodeGenTypes
exception NotImplemented
exception LogicError of string

type symbol = LocalVariable of int | GlobalVariable of int | FunctionParameter of int | ExtendFunction of int
and  symbolTable = symbol StringMap.t
and  symbolTableType = Locals | Globals | ExtendFunctions

let helper_functions = Hashtbl.create 10
let runtime_functions = Hashtbl.create 20

let index_map table_type m =
  let add_item key _ (accum_map, accum_idx) =
    let index_val = match table_type with Locals -> LocalVariable(accum_idx) | Globals -> GlobalVariable(accum_idx) | ExtendFunctions -> ExtendFunction(accum_idx) in
    (StringMap.add key index_val accum_map, accum_idx + 1) in
  StringMap.fold add_item m (StringMap.empty, 0)

let (=>) struct_ptr elem = (fun val_name builder ->
    let the_pointer = Llvm.build_struct_gep struct_ptr elem "the_pointer" builder in
    Llvm.build_load the_pointer val_name builder);;

let ($>) val_to_store (struct_ptr, elem)  = (fun builder ->
    let the_pointer = Llvm.build_struct_gep struct_ptr elem "" builder in
    Llvm.build_store val_to_store the_pointer builder);;

(* from http://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function without the infix *)
let zero_until i =
  let rec aux n acc =
    if n < 0 then acc else aux (n-1) (n :: acc)
  in aux (i-1) []

let create_runtime_functions ctx bt the_module =
  let add_runtime_func fname returntype arglist =
    let the_func = Llvm.declare_function fname (Llvm.function_type returntype arglist) the_module
    in Hashtbl.add runtime_functions fname the_func in
  add_runtime_func "strlen" bt.long_t [|bt.char_p|];
  add_runtime_func "strcmp" bt.long_t [|bt.char_p; bt.char_p|];
  add_runtime_func "pow" bt.float_t [|bt.float_t; bt.float_t|] ;
  add_runtime_func "lrint" bt.int_t [|bt.float_t|] ;
  add_runtime_func "llvm.memcpy.p0i8.p0i8.i64" bt.void_t [|bt.char_p; bt.char_p; bt.long_t; bt.int_t; bt.bool_t|] ;
  add_runtime_func "incStack" bt.void_t [||] ;
  add_runtime_func "getVal" bt.value_p [|bt.var_instance_p; bt.int_t; bt.int_t|] ;
  add_runtime_func "clone_value" bt.value_p [|bt.value_p;|] ;
  (* add_runtime_func "freeMe" (Llvm.void_type ctx) [|bt.extend_scope_p;|] ; *)
  add_runtime_func "getSize" bt.value_p [|bt.var_instance_p;|] ;
  add_runtime_func "get_variable" bt.var_instance_p [|bt.extend_scope_p; bt.int_t|] ;
  add_runtime_func "null_init" (Llvm.void_type ctx) [|bt.extend_scope_p|] ;
  add_runtime_func "debug_print" (Llvm.void_type ctx) [|bt.value_p ; bt.char_p|] ;
  add_runtime_func "new_string_go_all_the_way" bt.value_p [|bt.char_p|] ;
  add_runtime_func "deref_subrange_p" bt.value_p [|bt.subrange_p|];
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
    let llvm_fname = "extend_" ^ fname in
    let llvm_fn = Llvm.declare_function llvm_fname llvm_ftype base_module in
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
  let array_of_vardefn_ptrs = Llvm.define_global "array_of_vardefn_ptrs" (Llvm.const_array base_types.var_defn_p vardefn_array) base_module in

  (* Create the pointer to the global scope object *)
  let global_scope_loc = Llvm.define_global "global_scope_loc" (Llvm.const_pointer_null base_types.extend_scope_p) base_module in

  let main_def = Llvm.define_function "main" (Llvm.function_type base_types.int_t [|base_types.int_t; base_types.char_p_p|]) base_module in
  let main_bod = Llvm.builder_at_end context (Llvm.entry_block main_def) in

  (* Create the array of value_ps that will contain the responses to TypeOf(val) *)
  let null_val_ptr = Llvm.const_pointer_null base_types.value_p in
  let null_val_array = Array.make (Array.length int_to_type_array) null_val_ptr in
  let array_of_typeof_val_ptrs = Llvm.define_global "array_of_val_ptrs" (Llvm.const_array base_types.value_p null_val_array) base_module in
  let create_typeof_string i s =
    let sp = Llvm.build_global_stringptr s "global_typeof_stringptr" main_bod in
    let vp = Llvm.build_call (Hashtbl.find runtime_functions "new_string_go_all_the_way") [|sp|] "global_typeof_string" main_bod in
    let vp_dst = Llvm.build_in_bounds_gep array_of_typeof_val_ptrs [|Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t i|] ("global_typeof_dst") main_bod in
    let _ = Llvm.build_store vp vp_dst main_bod in
    () in
  Array.iteri create_typeof_string int_to_type_array ;

  (* Look these two up once and for all *)
  (* let deepCopy = Hashtbl.find runtime_functions "deepCopy" in *)
  (* let freeMe = Hashtbl.find runtime_functions "freeMe" in *)
  let getVal = Hashtbl.find runtime_functions "getVal" in (*getVal retrieves the value of a variable instance for a specific x and y*)
  let getVar = Hashtbl.find runtime_functions "get_variable" in (*getVar retrieves a variable instance based on the offset. It instanciates the variable if it does not exist yet*)

  (* build_formula_function takes a symbol table and an expression, builds the LLVM function, and returns the llvalue of the function *)
  let build_formula_function (varname, formula_idx) symbols formula_expr =
    let form_decl = Llvm.define_function ("formula_fn_" ^ varname ^ "_num_" ^ (string_of_int formula_idx)) base_types.formula_call_t base_module in
    let builder_at_top = Llvm.builder_at_end context (Llvm.entry_block form_decl) in
    let local_scope = Llvm.param form_decl 0 in
    let global_scope = Llvm.build_load global_scope_loc "global_scope" builder_at_top in

    (* Some repeated stuff to avoid cut & paste *)
    let empty_type = (Llvm.const_int base_types.char_t (value_field_flags_index Empty)) in
    let number_type = (Llvm.const_int base_types.char_t (value_field_flags_index Number)) in
    let string_type = (Llvm.const_int base_types.char_t (value_field_flags_index String)) in
    let range_type = (Llvm.const_int base_types.char_t (value_field_flags_index Range)) in
    let make_block blockname =
      let new_block = Llvm.append_block context blockname form_decl in
      let new_builder = Llvm.builder_at_end context new_block in
      (new_block, new_builder) in
    let store_number value_ptr store_builder number_llvalue =
      let sp = Llvm.build_struct_gep value_ptr (value_field_index Number) "num_pointer" store_builder in
      let _ = Llvm.build_store number_type (Llvm.build_struct_gep value_ptr (value_field_index Flags) "" store_builder) store_builder in
      ignore (Llvm.build_store number_llvalue sp store_builder) in
    let store_empty value_ptr store_builder =
      ignore (Llvm.build_store empty_type (Llvm.build_struct_gep value_ptr (value_field_index Flags) "" store_builder) store_builder) in

    let make_truthiness_blocks blockprefix ret_val =
      let (merge_bb, merge_builder) = make_block (blockprefix ^ "_merge") in

      let (make_true_bb, make_true_builder) = make_block (blockprefix ^ "_true") in
      let _ = store_number ret_val make_true_builder (Llvm.const_float base_types.float_t 1.0) in
      let _ = Llvm.build_br merge_bb make_true_builder in

      let (make_false_bb, make_false_builder) = make_block (blockprefix ^ "_false") in
      let _ = store_number ret_val make_false_builder (Llvm.const_float base_types.float_t 0.0) in
      let _ = Llvm.build_br merge_bb make_false_builder in

      let (make_empty_bb, make_empty_builder) = make_block (blockprefix ^ "_empty") in
      let _ = store_empty ret_val make_empty_builder  in
      let _ = Llvm.build_br merge_bb make_empty_builder in

      (make_true_bb, make_false_bb, make_empty_bb, merge_builder) in

    let rec build_expr old_builder exp = match exp with
        LitInt(i) -> let vvv = Llvm.const_float base_types.float_t (float_of_int i) in
        let ret_val = Llvm.build_malloc base_types.value_t "int_ret_val" old_builder in
        let _ = store_number ret_val old_builder vvv in
        (ret_val, old_builder)
      | LitFlt(f) -> let vvv = Llvm.const_float base_types.float_t f in
        let ret_val = Llvm.build_malloc base_types.value_t "flt_ret_val" old_builder in
        let _ = store_number ret_val old_builder vvv in
        (ret_val, old_builder)
      | Empty ->
        let ret_val = Llvm.build_malloc base_types.value_t "empty_ret_val" old_builder in
        let _ = store_empty ret_val old_builder in
        (ret_val, old_builder)
      | Debug(e) ->
        let (ret_val, new_builder) = build_expr old_builder e in
        let _ = Llvm.build_call (Hashtbl.find runtime_functions "debug_print") [|ret_val; Llvm.const_pointer_null base_types.char_p|] "" new_builder in
        (ret_val, new_builder)
      | Id(name) ->
        let create_and_deref_subrange appropriate_scope i =
          let llvm_var = Llvm.build_call getVar [|appropriate_scope; Llvm.const_int base_types.int_t i|] "llvm_var" old_builder in
          let base_var_num_rows = (llvm_var => (var_instance_field_index Rows)) "base_var_num_rows" old_builder in
          let base_var_num_cols = (llvm_var => (var_instance_field_index Cols)) "base_var_num_rows" old_builder in
          let subrange_ptr = Llvm.build_alloca base_types.subrange_t "subrange_ptr" old_builder in
          let _ = (llvm_var $> (subrange_ptr, (subrange_field_index BaseRangePtr))) old_builder in
          let _ = ((Llvm.const_null base_types.int_t) $> (subrange_ptr, (subrange_field_index BaseOffsetRow))) old_builder in
          let _ = ((Llvm.const_null base_types.int_t) $> (subrange_ptr, (subrange_field_index BaseOffsetCol))) old_builder in
          let _ = (base_var_num_rows $> (subrange_ptr, (subrange_field_index SubrangeRows))) old_builder in
          let _ = (base_var_num_cols $> (subrange_ptr, (subrange_field_index SubrangeCols))) old_builder in
          (Llvm.build_call (Hashtbl.find runtime_functions "deref_subrange_p") [|subrange_ptr|] "local_id_ret_val" old_builder, old_builder) in
        (
          match (try StringMap.find name symbols with Not_found -> raise(LogicError("Something went wrong with your semantic analysis - " ^ name ^ " not found"))) with
            LocalVariable(i) -> create_and_deref_subrange local_scope i
          | GlobalVariable(i) -> create_and_deref_subrange global_scope i
          | FunctionParameter(i) ->
            let paramarray = (local_scope => (scope_field_type_index FunctionParams)) "paramarray" old_builder in
            let param_addr = Llvm.build_in_bounds_gep paramarray [|Llvm.const_int base_types.int_t i|] "param_addr" old_builder in
            let param = Llvm.build_load param_addr "param" old_builder in
            (Llvm.build_call (Hashtbl.find runtime_functions "clone_value") [|param|] "function_param_ret_val" old_builder, old_builder)
          | ExtendFunction(i) -> raise(LogicError("Something went wrong with your semantic analyis - function " ^ name ^ " used as variable in RHS for " ^ varname))
        )
      | Selection(expr, sel) ->
        let (expr_val, expr_builder) = build_expr old_builder expr in
        let build_rhs_index idx_builder = function
            Abs(e) ->
            let (idx_expr_val, next_builder) = build_expr idx_builder e in
            let rhs_idx_ptr = Llvm.build_alloca base_types.rhs_index_t "idx_ptr" next_builder in
            let _ = (idx_expr_val $> (rhs_idx_ptr, (rhs_index_field_index RhsExprVal))) next_builder in
            let _ = ((Llvm.const_int base_types.char_t (rhs_index_type_flags_const RhsIdxAbs)) $> (rhs_idx_ptr, (rhs_index_field_index RhsIndexType))) next_builder in
            (rhs_idx_ptr, next_builder)
          | Rel(e) ->
            let (idx_expr_val, next_builder) = build_expr idx_builder e in
            let rhs_idx_ptr = Llvm.build_alloca base_types.rhs_index_t "idx_ptr" next_builder in
            let _ = (idx_expr_val $> (rhs_idx_ptr, (rhs_index_field_index RhsExprVal))) next_builder in
            let _ = ((Llvm.const_int base_types.char_t (rhs_index_type_flags_const RhsIdxRel)) $> (rhs_idx_ptr, (rhs_index_field_index RhsIndexType))) next_builder in
            (rhs_idx_ptr, next_builder)
          | DimensionStart ->
            let rhs_idx_ptr = Llvm.build_alloca base_types.rhs_index_t "idx_ptr" idx_builder in
            let _ = ((Llvm.const_pointer_null base_types.value_p) $> (rhs_idx_ptr, (rhs_index_field_index RhsExprVal))) idx_builder in
            let _ = ((Llvm.const_int base_types.char_t (rhs_index_type_flags_const RhsIdxDimStart)) $> (rhs_idx_ptr, (rhs_index_field_index RhsIndexType))) idx_builder in
            (rhs_idx_ptr, idx_builder)
          | DimensionEnd ->
            let rhs_idx_ptr = Llvm.build_alloca base_types.rhs_index_t "idx_ptr" idx_builder in
            let _ = ((Llvm.const_pointer_null base_types.value_p) $> (rhs_idx_ptr, (rhs_index_field_index RhsExprVal))) idx_builder in
            let _ = ((Llvm.const_int base_types.char_t (rhs_index_type_flags_const RhsIdxDimEnd)) $> (rhs_idx_ptr, (rhs_index_field_index RhsIndexType))) idx_builder in
            (rhs_idx_ptr, idx_builder) in
        let build_rhs_slice slice_builder = function
            (Some start_idx, Some end_idx) ->
            let rhs_slice_ptr = Llvm.build_alloca base_types.rhs_slice_t "slice_ptr" slice_builder in
            let (start_idx_ptr, next_builder) = build_rhs_index slice_builder start_idx in
            let (end_idx_ptr, last_builder) = build_rhs_index next_builder end_idx in
            let _ = (start_idx_ptr $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceStartIdx))) last_builder in
            let _ = (end_idx_ptr $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceEndIdx))) last_builder in
            (rhs_slice_ptr,last_builder)
          | (Some single_idx, None) ->
            let rhs_slice_ptr = Llvm.build_alloca base_types.rhs_slice_t "slice_ptr" slice_builder in
            let (single_idx_ptr, last_builder) = build_rhs_index slice_builder single_idx in
            let _ = (single_idx_ptr $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceStartIdx))) last_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_index_p) $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceEndIdx))) last_builder in
            (rhs_slice_ptr,last_builder)
          | (None, None) ->
            let rhs_slice_ptr = Llvm.build_alloca base_types.rhs_slice_t "slice_ptr" slice_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_index_p) $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceStartIdx))) slice_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_index_p) $> (rhs_slice_ptr, (rhs_slice_field_index RhsSliceEndIdx))) slice_builder in
            (rhs_slice_ptr,slice_builder)
          | (None, Some illegal_idx) -> print_endline (string_of_expr exp) ; raise (LogicError("This slice should not be grammatically possible")) in
        let build_rhs_sel sel_builder = function
            (Some first_slice, Some second_slice) ->
            let rhs_selection_ptr = Llvm.build_alloca base_types.rhs_selection_t "selection_ptr" sel_builder in
            let (first_slice_ptr, next_builder) = build_rhs_slice sel_builder first_slice in
            let (second_slice_ptr, last_builder) = build_rhs_slice next_builder second_slice in
            let _ = (first_slice_ptr $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice1))) last_builder in
            let _ = (second_slice_ptr $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice2))) last_builder in
            (rhs_selection_ptr,last_builder)
          | (Some single_slice, None) ->
            let rhs_selection_ptr = Llvm.build_alloca base_types.rhs_selection_t "selection_ptr" sel_builder in
            let (single_slice_ptr, last_builder) = build_rhs_slice sel_builder single_slice in
            let _ = (single_slice_ptr $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice1))) last_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_slice_p) $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice2))) last_builder in
            (rhs_selection_ptr,last_builder)
          | (None, None) ->
            let rhs_selection_ptr = Llvm.build_alloca base_types.rhs_selection_t "selection_ptr" sel_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_slice_p) $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice1))) sel_builder in
            let _ = ((Llvm.const_pointer_null base_types.rhs_slice_p) $> (rhs_selection_ptr, (rhs_selection_field_index RhsSelSlice2))) sel_builder in
            (rhs_selection_ptr,sel_builder)
          | (None, Some illegal_idx) -> print_endline (string_of_expr exp) ; raise (LogicError("This slice should not be grammatically possible")) in
        let (selection_ptr, builder_to_end_all_builders) = build_rhs_sel expr_builder sel in
        (expr_val, builder_to_end_all_builders)
      | Precedence(a,b) -> let (_, new_builder) = build_expr old_builder a in build_expr new_builder b
      | LitString(str) ->
        let boxxx = Llvm.build_call
            (Hashtbl.find helper_functions "new_string")
            (Array.of_list [
                Llvm.build_global_stringptr str "glob_str" old_builder
              ]) "boxed_str" old_builder in
        let boxx = Llvm.build_call
            (Hashtbl.find helper_functions "box_value_string")
            (Array.of_list [boxxx]) "box_value_str" old_builder
        in (boxx, old_builder)
      | Call(fn,exl) -> (*TODO: Call needs to be reviewed. Possibly switch call arguments to value_p*)
        let build_one_expr (arg_list, intermediate_builder) e =
          let (arg_val, next_builder) = build_expr intermediate_builder e in
          (arg_val :: arg_list, next_builder) in
        let (reversed_arglist, call_builder) = List.fold_left build_one_expr ([], old_builder) exl in
        let args = Array.of_list (List.rev reversed_arglist) in
        let result = Llvm.build_call (
          StringMap.find fn function_llvalues
          ) args "call_ret_val" call_builder in
        (result, call_builder)
      | BinOp(expr1,op,expr2) -> (
          let (val1, builder1) = build_expr old_builder expr1 in
          let (val2, int_builder) = build_expr builder1 expr2 in
          let bit_shift = (Llvm.const_int base_types.char_t 4) in
          let expr1_type = (val1 => (value_field_index Flags)) "expr1_type" int_builder in
          let expr2_type = (val2 => (value_field_index Flags)) "expr2_type" int_builder in
          let expr1_type_shifted = Llvm.build_shl expr1_type bit_shift "expr_1_type_shifted" int_builder in
          let combined_type = Llvm.build_add expr1_type_shifted expr2_type "combined_type" int_builder in
          let number_number = Llvm.const_add (Llvm.const_shl number_type bit_shift) number_type in
          let string_string = Llvm.const_add (Llvm.const_shl string_type bit_shift) string_type in
          let empty_empty = Llvm.const_add (Llvm.const_shl empty_type bit_shift) empty_type in
          let range_range = Llvm.const_add (Llvm.const_shl range_type bit_shift) range_type in
          let build_simple_binop oppp int_builder =
            (let ret_val = Llvm.build_malloc base_types.value_t "binop_minus_ret_val" int_builder in
              let _ = Llvm.build_store
                  (
                    Llvm.const_int
                    base_types.char_t
                    (value_field_flags_index Empty)
                  ) (
                    Llvm.build_struct_gep
                    ret_val
                    (value_field_index Flags)
                    ""
                    int_builder
                  )
                  int_builder
              in
              let bailout = (Llvm.append_block context "" form_decl) in
              let bbailout = Llvm.builder_at_end context bailout in
              let (numnum_bb, numnum_builder) = make_block "numnum" in
              let numeric_val_1 = (val1 => (value_field_index Number)) "number_one" numnum_builder in
              let numeric_val_2 = (val2 => (value_field_index Number)) "number_two" numnum_builder in
              let numeric_res = oppp numeric_val_1 numeric_val_2 "numeric_res" numnum_builder in
              let _ = Llvm.build_store
                  numeric_res (
                    Llvm.build_struct_gep
                    ret_val
                    (value_field_index Number)
                    ""
                    numnum_builder
                  )
                  numnum_builder in
              let _ = Llvm.build_store
                  (
                    Llvm.const_int
                    base_types.char_t
                    (value_field_flags_index Number)
                  ) (
                    Llvm.build_struct_gep
                    ret_val
                    (value_field_index Flags)
                    ""
                    numnum_builder
                  )
                  numnum_builder in
              let _ = Llvm.build_br bailout numnum_builder in
              let _ = Llvm.build_cond_br (Llvm.build_icmp Llvm.Icmp.Eq combined_type number_number "" int_builder) numnum_bb bailout int_builder in
               (ret_val, bbailout)
           )
           and build_simple_int_binop oppp int_builder =
             (let ret_val = Llvm.build_malloc base_types.value_t "binop_minus_ret_val" int_builder in
               let _ = Llvm.build_store
                   (
                     Llvm.const_int
                     base_types.char_t
                     (value_field_flags_index Empty)
                   ) (
                     Llvm.build_struct_gep
                     ret_val
                     (value_field_index Flags)
                     ""
                     int_builder
                   )
                   int_builder
               in
               let bailout = (Llvm.append_block context "" form_decl) in
               let bbailout = Llvm.builder_at_end context bailout in
               let (numnum_bb, numnum_builder) = make_block "numnum" in
               let roundfl x = Llvm.build_call (Hashtbl.find runtime_functions "lrint") [|x|] "" numnum_builder in
               let numeric_val_1 = roundfl ((val1 => (value_field_index Number)) "number_one" numnum_builder) in
               let numeric_val_2 = roundfl ((val2 => (value_field_index Number)) "number_two" numnum_builder) in
               let numeric_res = oppp numeric_val_1 numeric_val_2 "numeric_res" numnum_builder in
               let _ = Llvm.build_store
                   (Llvm.build_sitofp numeric_res base_types.float_t "" numnum_builder)
                   (
                     Llvm.build_struct_gep
                     ret_val
                     (value_field_index Number)
                     ""
                     numnum_builder
                   )
                   numnum_builder in
               let _ = Llvm.build_store
                   (
                     Llvm.const_int
                     base_types.char_t
                     (value_field_flags_index Number)
                   ) (
                     Llvm.build_struct_gep
                     ret_val
                     (value_field_index Flags)
                     ""
                     numnum_builder
                   )
                   numnum_builder in
               let _ = Llvm.build_br bailout numnum_builder in
               let _ = Llvm.build_cond_br (Llvm.build_icmp Llvm.Icmp.Eq combined_type number_number "" int_builder) numnum_bb bailout int_builder in
                (ret_val, bbailout)
             ) in
          let build_boolean_op numeric_comparator string_comparator int_builder =
            let ret_val = Llvm.build_malloc base_types.value_t "binop_gt_ret_val" int_builder in
            let (make_true_bb, make_false_bb, make_empty_bb, merge_builder) = make_truthiness_blocks "binop_eq" ret_val in

            let (numnum_bb, numnum_builder) = make_block "numnum" in
            let numeric_val_1 = (val1 => (value_field_index Number)) "number_one" numnum_builder in
            let numeric_val_2 = (val2 => (value_field_index Number)) "number_two" numnum_builder in
            let numeric_greater = Llvm.build_fcmp numeric_comparator numeric_val_1 numeric_val_2 "numeric_greater" numnum_builder in
            let _ = Llvm.build_cond_br numeric_greater make_true_bb make_false_bb numnum_builder in

            let (strstr_bb, strstr_builder) = make_block "strstr" in
            let str_p_1 = (val1 => (value_field_index String)) "string_one" strstr_builder in
            let str_p_2 = (val2 => (value_field_index String)) "string_two" strstr_builder in
            let char_p_1 = (str_p_1 => (string_field_index StringCharPtr)) "char_p_one" strstr_builder in
            let char_p_2 = (str_p_2 => (string_field_index StringCharPtr)) "char_p_two" strstr_builder in
            let strcmp_result = Llvm.build_call (Hashtbl.find runtime_functions "strcmp") [|char_p_1; char_p_2|] "strcmp_result" strstr_builder in
            let string_greater = Llvm.build_icmp string_comparator strcmp_result (Llvm.const_null base_types.long_t) "string_greater" strstr_builder in
            let _ = Llvm.build_cond_br string_greater make_true_bb make_false_bb strstr_builder in

            let switch_inst = Llvm.build_switch combined_type make_empty_bb 2 int_builder in (* Incompatible ===> default to empty *)
            Llvm.add_case switch_inst number_number numnum_bb;
            Llvm.add_case switch_inst string_string strstr_bb;
            (ret_val, merge_builder) in
          match op with
            Minus -> build_simple_binop Llvm.build_fsub int_builder
          | Plus ->
              let result = Llvm.build_malloc base_types.value_t "" int_builder
              and stradd = (Llvm.append_block context "" form_decl)
              and numadd = (Llvm.append_block context "" form_decl)
              and bailout = (Llvm.append_block context "" form_decl)
              and numorstrorother = (Llvm.append_block context "" form_decl)
              and strorother = (Llvm.append_block context "" form_decl)
              in
              let bstradd = Llvm.builder_at_end context stradd
              and bnumadd = Llvm.builder_at_end context numadd
              and bnumorstrorother = Llvm.builder_at_end context numorstrorother
              and bstrorother = Llvm.builder_at_end context strorother
              and bbailout = Llvm.builder_at_end context bailout
              and _ = Llvm.build_store
                  (
                    Llvm.const_int
                    base_types.char_t
                    (value_field_flags_index Empty)
                  ) (
                    Llvm.build_struct_gep
                    result
                    (value_field_index Flags)
                    ""
                    int_builder
                  )
                  int_builder
              in
              (*let _ = Llvm.build_cond_br pred_bool body_bb merge_bb pred_builder in*)
              let isnumber = Llvm.build_icmp
                  Llvm.Icmp.Eq
                  (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val1
                      (value_field_index Flags)
                      ""
                      bnumorstrorother
                    ) "" bnumorstrorother
                  ) (
                    Llvm.const_int
                    base_types.char_t
                    (value_field_flags_index Number)
                  )
                  ""
                  bnumorstrorother
              and isstring = Llvm.build_icmp
                  Llvm.Icmp.Eq
                  (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val1
                      (value_field_index Flags)
                      ""
                      bstrorother
                    )
                    ""
                    bstrorother
                  ) (
                    Llvm.const_int
                    base_types.char_t
                    (value_field_flags_index String)
                  )
                  ""
                  bstrorother
              and isnumorstring = Llvm.build_icmp
                  Llvm.Icmp.Eq
                  (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val1
                      (value_field_index Flags)
                      ""
                      int_builder
                    )
                    ""
                    int_builder
                  ) (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val2
                      (value_field_index Flags)
                      ""
                      int_builder
                    )
                    ""
                    int_builder
                  )
                  ""
                  int_builder
              and _ = Llvm.build_store (
                  Llvm.build_fadd
                  (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val1
                      (value_field_index Number)
                      ""
                      bnumadd
                    )
                    ""
                    bnumadd
                  ) (
                    Llvm.build_load
                    (
                      Llvm.build_struct_gep
                      val2
                      (value_field_index Number)
                      ""
                      bnumadd
                    )
                    ""
                    bnumadd
                  )
                  ""
                  bnumadd
                ) (
                  Llvm.build_struct_gep
                  result
                  (value_field_index Number)
                  ""
                  bnumadd
                )
                bnumadd
              and _ = Llvm.build_store (
                  Llvm.const_int base_types.char_t (value_field_flags_index Number)
                ) (
                  Llvm.build_struct_gep
                  result
                  (value_field_index Flags)
                  ""
                  bnumadd
                )
                bnumadd
              and str1 = Llvm.build_load
              (
                Llvm.build_struct_gep
                val1
                (value_field_index String)
                ""
                bstradd
              ) "" bstradd
              and str2 = Llvm.build_load
              (
                Llvm.build_struct_gep
                val2
                (value_field_index String)
                ""
                bstradd
              ) "" bstradd
              and newstr =
              (
                Llvm.build_malloc base_types.string_t "" bstradd
              )
              in
              let len1 = Llvm.build_load (
                Llvm.build_struct_gep
                str1
                (string_field_index StringLen)
                ""
                bstradd
              ) "" bstradd
              and len2 = Llvm.build_load (
                Llvm.build_struct_gep
                str2
                (string_field_index StringLen)
                ""
                bstradd
              ) "" bstradd
              and p1 = Llvm.build_load (
                Llvm.build_struct_gep
                str1
                (string_field_index StringCharPtr)
                ""
                bstradd
              ) "" bstradd
              and p2 = Llvm.build_load (
                Llvm.build_struct_gep
                str2
                (string_field_index StringCharPtr)
                ""
                bstradd
              ) "" bstradd
              and dst_char_ptr_ptr = (
                Llvm.build_struct_gep
                newstr
                (string_field_index StringCharPtr)
                ""
                bstradd
              )
              and _ = Llvm.build_store (
                Llvm.const_int base_types.char_t (value_field_flags_index String)
              ) (
                Llvm.build_struct_gep
                result
                (value_field_index Flags)
                ""
                bstradd
              ) bstradd
              and _ = Llvm.build_store newstr (
                Llvm.build_struct_gep
                result
                (value_field_index String)
                ""
                bstradd
              )
              bstradd in
              let fullLen = Llvm.build_nsw_add (Llvm.build_nsw_add len1 len2 "" bstradd) (Llvm.const_int base_types.long_t 1) "" bstradd
              and extra_byte2 = (Llvm.build_add len2 (Llvm.const_int base_types.long_t 1) "" bstradd) in
              let dst_char = Llvm.build_array_malloc base_types.char_t (Llvm.build_trunc fullLen base_types.int_t "" bstradd) "" bstradd in
              let dst_char2 = Llvm.build_in_bounds_gep dst_char [|len1|] "" bstradd in
              let _ = Llvm.build_call
                (Hashtbl.find runtime_functions "llvm.memcpy.p0i8.p0i8.i64")
                [|dst_char; p1; len1; (Llvm.const_int base_types.int_t 0); (Llvm.const_int base_types.bool_t 0)|]
                ""
                bstradd
              and _ = Llvm.build_call
                (Hashtbl.find runtime_functions "llvm.memcpy.p0i8.p0i8.i64")
                [|dst_char2; p2; extra_byte2; (Llvm.const_int base_types.int_t 0); (Llvm.const_int base_types.bool_t 0)|]
                ""
                bstradd
              and _ = Llvm.build_store dst_char dst_char_ptr_ptr bstradd
              in
              let _ = Llvm.build_store (Llvm.build_nsw_add fullLen (Llvm.const_int base_types.long_t (-1)) "" bstradd) (Llvm.build_struct_gep newstr (string_field_index StringLen) "" bstradd) bstradd
              in
              let _ = Llvm.build_cond_br isnumorstring numorstrorother bailout int_builder
              and _ = Llvm.build_cond_br isnumber numadd strorother bnumorstrorother
              and _ = Llvm.build_cond_br isstring stradd bailout bstrorother
              and _ = Llvm.build_br bailout bstradd
              and _ = Llvm.build_br bailout bnumadd
              in
              (result, bbailout)
          | Times -> build_simple_binop Llvm.build_fmul int_builder
          | Eq ->
            (* let _ = Llvm.build_call (Hashtbl.find runtime_functions "debug_print") [|val1; Llvm.build_global_stringptr "Eq operator - value 1" "" old_builder|] "" int_builder in
            let _ = Llvm.build_call (Hashtbl.find runtime_functions "debug_print") [|val2; Llvm.build_global_stringptr "Eq operator - value 2" "" old_builder|] "" int_builder in *)
            let ret_val = Llvm.build_malloc base_types.value_t "binop_eq_ret_val" int_builder in
            let (make_true_bb, make_false_bb, _, merge_builder) = make_truthiness_blocks "binop_eq" ret_val in

            let (numnum_bb, numnum_builder) = make_block "numnum" in
            let numeric_val_1 = (val1 => (value_field_index Number)) "number_one" numnum_builder in
            let numeric_val_2 = (val2 => (value_field_index Number)) "number_two" numnum_builder in
            let numeric_equality = Llvm.build_fcmp Llvm.Fcmp.Oeq numeric_val_1 numeric_val_2 "numeric_equality" numnum_builder in
            let _ = Llvm.build_cond_br numeric_equality make_true_bb make_false_bb numnum_builder in

            let (strstr_bb, strstr_builder) = make_block "strstr" in
            let str_p_1 = (val1 => (value_field_index String)) "string_one" strstr_builder in
            let str_p_2 = (val2 => (value_field_index String)) "string_two" strstr_builder in
            let char_p_1 = (str_p_1 => (string_field_index StringCharPtr)) "char_p_one" strstr_builder in
            let char_p_2 = (str_p_2 => (string_field_index StringCharPtr)) "char_p_two" strstr_builder in
            let strcmp_result = Llvm.build_call (Hashtbl.find runtime_functions "strcmp") [|char_p_1; char_p_2|] "strcmp_result" strstr_builder in
            let string_equality = Llvm.build_icmp Llvm.Icmp.Eq strcmp_result (Llvm.const_null base_types.long_t) "string_equality" strstr_builder in
            let _ = Llvm.build_cond_br string_equality make_true_bb make_false_bb strstr_builder in

            let (rngrng_bb, rngrng_builder) = make_block "rngrng" in
            (* TODO: Make this case work *)
            let _ = Llvm.build_br make_false_bb rngrng_builder in

            let switch_inst = Llvm.build_switch combined_type make_false_bb 4 int_builder in (* Incompatible ===> default to false *)
            Llvm.add_case switch_inst number_number numnum_bb;
            Llvm.add_case switch_inst string_string strstr_bb;
            Llvm.add_case switch_inst range_range rngrng_bb;
            Llvm.add_case switch_inst empty_empty make_true_bb; (* Nothing to check in this case, just return true *)
            (ret_val, merge_builder)
          | Gt -> build_boolean_op Llvm.Fcmp.Ogt Llvm.Icmp.Sgt int_builder
          | GtEq -> build_boolean_op Llvm.Fcmp.Oge Llvm.Icmp.Sge int_builder
          | Lt -> build_boolean_op Llvm.Fcmp.Olt Llvm.Icmp.Slt int_builder
          | LtEq -> build_boolean_op Llvm.Fcmp.Ole Llvm.Icmp.Sle int_builder
          | LogAnd | LogOr -> raise (TransformedAway("&& and || should have been transformed into a short-circuit ternary expression! Error in the following expression:\n" ^ string_of_expr exp))
          | Divide-> build_simple_binop Llvm.build_fdiv int_builder
          | Mod-> build_simple_binop Llvm.build_frem int_builder
          | Pow-> (
            let powcall numeric_val_1 numeric_val_2 valname b =
              Llvm.build_call (Hashtbl.find runtime_functions "pow") [|numeric_val_1; numeric_val_2|] "" b in
            build_simple_binop powcall int_builder)
          | LShift-> build_simple_int_binop Llvm.build_shl int_builder
          | RShift-> build_simple_int_binop Llvm.build_lshr int_builder
          | BitOr-> build_simple_int_binop Llvm.build_or int_builder
          | BitAnd-> build_simple_int_binop Llvm.build_and int_builder
          | BitXor-> build_simple_int_binop Llvm.build_xor int_builder
        )
      | UnOp(SizeOf,expr) -> let vvv = Llvm.const_float base_types.float_t 0.0 in
        let ret_val = Llvm.build_malloc base_types.value_t "unop_size_ret_val" old_builder in
        let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" old_builder in
        let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" old_builder) old_builder in
        let _ = Llvm.build_store vvv sp old_builder in
        (ret_val, old_builder)
      | UnOp(Truthy, expr) ->
        let ret_val = Llvm.build_malloc base_types.value_t "unop_truthy_ret_val" old_builder in
        let (expr_val, expr_builder) = build_expr old_builder expr in

        let (truthy_bb, falsey_bb, empty_bb, merge_builder) = make_truthiness_blocks "binop_eq" ret_val in

        let expr_flags = (expr_val => (value_field_index Flags)) "expr_flags" expr_builder in
        let is_empty_bool = (Llvm.build_icmp Llvm.Icmp.Eq expr_flags (Llvm.const_int base_types.flags_t (value_field_flags_index Empty)) "is_empty_bool" expr_builder) in
        let is_empty = Llvm.build_zext is_empty_bool base_types.char_t "is_empty" expr_builder in
        let is_empty_two = Llvm.build_shl is_empty (Llvm.const_int base_types.char_t 1) "is_empty_two" expr_builder in
        let is_number = Llvm.build_icmp Llvm.Icmp.Eq expr_flags (Llvm.const_int base_types.flags_t (value_field_flags_index Number)) "is_number" expr_builder in
        let the_number = (expr_val => (value_field_index Number)) "the_number" expr_builder in
        let is_zero = Llvm.build_fcmp Llvm.Fcmp.Oeq the_number (Llvm.const_float base_types.number_t 0.0) "is_zero" expr_builder in
        let is_numeric_zero_bool = Llvm.build_and is_zero is_number "is_numeric_zero_bool" expr_builder in
        let is_numeric_zero = Llvm.build_zext is_numeric_zero_bool base_types.char_t "is_numeric_zero" expr_builder in
        let switch_num = Llvm.build_add is_empty_two is_numeric_zero "switch_num" expr_builder in
        let switch_inst = Llvm.build_switch switch_num empty_bb 2 expr_builder in
        Llvm.add_case switch_inst (Llvm.const_int base_types.char_t 0) truthy_bb; (* empty << 1 + is_zero == 0 ===> truthy *)
        Llvm.add_case switch_inst (Llvm.const_int base_types.char_t 1) falsey_bb; (* empty << 1 + is_zero == 1 ===> falsey *)
        (ret_val, merge_builder)
      | UnOp(LogNot, expr) ->
        let (truth_val, truth_builder) = build_expr old_builder (UnOp(Truthy, expr)) in
        let the_number = (truth_val => (value_field_index Number)) "the_number" truth_builder in
        let not_the_number = Llvm.build_fsub (Llvm.const_float base_types.float_t 1.0) the_number "not_the_number" truth_builder in
        let sp = Llvm.build_struct_gep truth_val (value_field_index Number) "num_pointer" truth_builder in
        let _ = Llvm.build_store not_the_number sp truth_builder in
        (truth_val, truth_builder)
      | UnOp(Neg, expr) ->
        let ret_val = Llvm.build_malloc base_types.value_t "unop_truthy_ret_val" old_builder in
        let _ = store_empty ret_val old_builder in
        let (expr_val, expr_builder) = build_expr old_builder expr in
        let expr_type = (expr_val => (value_field_index Flags)) "expr_type" expr_builder in
        let is_number = Llvm.build_icmp Llvm.Icmp.Eq expr_type number_type "is_number" expr_builder in
        let (finish_bb, finish_builder) = make_block "finish" in

        let (number_bb, number_builder) = make_block "number" in
        let the_number = (expr_val => (value_field_index Number)) "the_number" number_builder in
        let minus_the_number = Llvm.build_fneg the_number "minus_the_number" number_builder in
        let _ = store_number ret_val number_builder minus_the_number in
        let _ = Llvm.build_br finish_bb number_builder in

        let _ = Llvm.build_cond_br is_number number_bb finish_bb expr_builder in
        (ret_val, finish_builder)
      | UnOp(BitNot, expr) -> print_endline "Unsupported Unop" ; print_endline (Ast.string_of_expr exp); raise NotImplemented
      | UnOp(TypeOf, expr) ->
        let (expr_val, expr_builder) = build_expr old_builder expr in
        let expr_type = (expr_val => (value_field_index Flags)) "expr_type" expr_builder in
        let vp_to_clone_loc = Llvm.build_in_bounds_gep array_of_typeof_val_ptrs [|Llvm.const_int base_types.int_t 0; expr_type|] ("vp_to_clone_log") expr_builder in
        let vp_to_clone = Llvm.build_load vp_to_clone_loc "vp_to_clone" expr_builder in
        let ret_val = Llvm.build_call (Hashtbl.find runtime_functions "clone_value") [|vp_to_clone|] "typeof_ret_val" expr_builder in
        (ret_val, expr_builder)
      | UnOp(Row, expr) -> print_endline "Unsupported Unop" ; print_endline (Ast.string_of_expr exp); raise NotImplemented
      | UnOp(Column, expr) -> print_endline "Unsupported Unop" ; print_endline (Ast.string_of_expr exp); raise NotImplemented
      | ReducedTernary(cond_var, true_var, false_var) ->
        let ret_val_addr = Llvm.build_alloca base_types.value_p "tern_ret_val_addr" old_builder in
        let (cond_val, _) = build_expr old_builder (Id(cond_var)) in (* Relying here on the fact that Id() doesn't change the builder *)
        let merge_bb = Llvm.append_block context "merge" form_decl in
        let merge_builder = Llvm.builder_at_end context merge_bb in
        let ret_val = Llvm.build_load ret_val_addr "tern_ret_val" merge_builder in

        let truthy_bb = Llvm.append_block context "truthy" form_decl in
        let truthy_builder = Llvm.builder_at_end context truthy_bb in
        let (truthy_val, _) = build_expr truthy_builder (Id(true_var)) in (* Relying here on the fact that Id() doesn't change the builder *)
        let _ = Llvm.build_store truthy_val ret_val_addr truthy_builder in
        let _ = Llvm.build_br merge_bb truthy_builder in

        let falsey_bb = Llvm.append_block context "falsey" form_decl in
        let falsey_builder = Llvm.builder_at_end context falsey_bb in
        let (falsey_val, _) = build_expr falsey_builder (Id(false_var)) in (* Relying here on the fact that Id() doesn't change the builder *)
        let _ = Llvm.build_store falsey_val ret_val_addr falsey_builder in
        let _ = Llvm.build_br merge_bb falsey_builder in

        let empty_bb = Llvm.append_block context "empty" form_decl in
        let empty_builder = Llvm.builder_at_end context empty_bb in
        let ret_val_empty = Llvm.build_malloc base_types.value_t "tern_empty" empty_builder in
        let _ = store_empty ret_val_empty empty_builder in
        let _ = Llvm.build_store ret_val_empty ret_val_addr empty_builder in
        let _ = Llvm.build_br merge_bb empty_builder in

        let expr_flags = (cond_val => (value_field_index Flags)) "expr_flags" old_builder in
        let is_empty_bool = (Llvm.build_icmp Llvm.Icmp.Eq expr_flags (Llvm.const_int base_types.flags_t (value_field_flags_index Empty)) "is_empty_bool" old_builder) in
        let is_empty = Llvm.build_zext is_empty_bool base_types.char_t "is_empty" old_builder in
        let is_empty_two = Llvm.build_shl is_empty (Llvm.const_int base_types.char_t 1) "is_empty_two" old_builder in
        let is_number = Llvm.build_icmp Llvm.Icmp.Eq expr_flags (Llvm.const_int base_types.flags_t (value_field_flags_index Number)) "is_number" old_builder in
        let the_number = (cond_val => (value_field_index Number)) "the_number" old_builder in
        let is_zero = Llvm.build_fcmp Llvm.Fcmp.Oeq the_number (Llvm.const_float base_types.number_t 0.0) "is_zero" old_builder in
        let is_numeric_zero_bool = Llvm.build_and is_zero is_number "is_numeric_zero_bool" old_builder in
        let is_numeric_zero = Llvm.build_zext is_numeric_zero_bool base_types.char_t "is_numeric_zero" old_builder in
        let switch_num = Llvm.build_add is_empty_two is_numeric_zero "switch_num" old_builder in
        let switch_inst = Llvm.build_switch switch_num empty_bb 2 old_builder in
        Llvm.add_case switch_inst (Llvm.const_int base_types.char_t 0) truthy_bb; (* empty << 1 + is_zero == 0 ===> truthy *)
        Llvm.add_case switch_inst (Llvm.const_int base_types.char_t 1) falsey_bb; (* empty << 1 + is_zero == 1 ===> falsey *)
        (ret_val, merge_builder)
      | unknown_expr -> print_endline (string_of_expr unknown_expr);raise NotImplemented in
    let (ret_value_p, final_builder) = build_expr builder_at_top formula_expr in
    let _ = Llvm.build_ret ret_value_p final_builder in
    form_decl in

  (*build formula creates a formula declaration in a separate method from the function it belongs to*)
  let build_formula (varname, idx) formula_array element symbols =
    let storage_addr = Llvm.build_in_bounds_gep formula_array [|Llvm.const_int base_types.int_t idx|] "" main_bod in
    let getStarts = function (* Not really just for starts *)
        Abs(LitInt(1)) | Abs(LitInt(0)) | DimensionStart | DimensionEnd -> (1, -1)
      | Abs(Id(s)) ->
        (match StringMap.find s symbols with
           LocalVariable(i) | GlobalVariable(i) -> (0, i)
         | _ -> raise(TransformedAway("Error in " ^ varname ^ ": The LHS expresssions should always either have dimension length 1 or be the name of a variable in their own scope.")))
      | _ -> print_endline ("Error in " ^ varname ^ " formula number " ^ string_of_int idx); raise(LogicError("Something wrong with the index of formula: " ^ string_of_formula element)) in
    let getEnds = function
        Some x -> let (b, c) = getStarts x in (b, c, 0)
      | None -> (0, -1, 1) in
    let (fromStartRow, rowStartVarnum) = getStarts element.formula_row_start in
    let (fromStartCol, colStartVarnum) = getStarts element.formula_col_start in
    let (toEndRow, rowEndVarnum, isSingleRow) = getEnds element.formula_row_end in
    let (toEndCol, colEndVarnum, isSingleCol) = getEnds element.formula_col_end in

    let _ = Llvm.build_store (Llvm.const_int base_types.char_t fromStartRow) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstRow) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t rowStartVarnum) (Llvm.build_struct_gep storage_addr (formula_field_index RowStartNum) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.char_t toEndRow) (Llvm.build_struct_gep storage_addr (formula_field_index ToLastRow) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t rowEndVarnum) (Llvm.build_struct_gep storage_addr (formula_field_index RowEndNum) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.char_t isSingleRow) (Llvm.build_struct_gep storage_addr (formula_field_index IsSingleRow) "" main_bod) main_bod in

    let _ = Llvm.build_store (Llvm.const_int base_types.char_t fromStartCol) (Llvm.build_struct_gep storage_addr (formula_field_index FromFirstCols) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t colStartVarnum) (Llvm.build_struct_gep storage_addr (formula_field_index ColStartNum) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.char_t toEndCol) (Llvm.build_struct_gep storage_addr (formula_field_index ToLastCol) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t colEndVarnum) (Llvm.build_struct_gep storage_addr (formula_field_index ColEndNum) "" main_bod) main_bod in
    let _ = Llvm.build_store (Llvm.const_int base_types.char_t isSingleCol) (Llvm.build_struct_gep storage_addr (formula_field_index IsSingleCol) "" main_bod) main_bod in

    let form_decl = build_formula_function (varname, idx) symbols element.formula_expr in
    let _ = Llvm.build_store form_decl (Llvm.build_struct_gep storage_addr (formula_field_index FormulaCall) "" main_bod) main_bod in
    () in

  (* Builds a var_defn struct for each variable *)
  let build_var_defn defn varname va symbols =
    let numForm = List.length va.var_formulas in
    let formulas = Llvm.build_array_malloc base_types.formula_t (Llvm.const_int base_types.int_t numForm) "" main_bod in
    (*getDefn simply looks up the correct definition for a dimension declaration of a variable. Note that currently it is ambiguous whether it is a variable or a literal. TOOD: consider negative numbers*)
    let getDefn = function
        DimId(a) -> (match StringMap.find a symbols with LocalVariable(i) -> i | GlobalVariable(i) -> i | _ -> raise(TransformedAway("Error in " ^ varname ^ ": The LHS expresssions should always either have dimension length 1 or be the name of a variable in their own scope.")))
      | DimInt(1) -> 1
      | DimInt(_) -> print_endline "Non1Dim" ; raise(NotImplemented) in
    let _ = (match va.var_rows with
          DimInt(1) -> Llvm.build_store (Llvm.const_int base_types.char_t 1) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" main_bod) main_bod
        | DimInt(_) -> print_endline "Non1Dim" ; raise(NotImplemented)
        | DimId(a) -> (
            let _ = Llvm.build_store (Llvm.const_int base_types.char_t 0) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" main_bod) main_bod in ();
            let _ = Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_rows)) (Llvm.build_struct_gep defn (var_defn_field_index Rows) "" main_bod) main_bod in ();
            Llvm.build_store (Llvm.const_int base_types.int_t (getDefn va.var_cols)) (Llvm.build_struct_gep defn (var_defn_field_index Cols) "" main_bod) main_bod
          )
      ) in
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t numForm) (Llvm.build_struct_gep defn (var_defn_field_index NumFormulas) "" main_bod) main_bod
    and _ = Llvm.build_store formulas (Llvm.build_struct_gep defn (var_defn_field_index Formulas) "" main_bod) main_bod
    and _ = Llvm.build_store (Llvm.build_global_stringptr varname "" main_bod) (Llvm.build_struct_gep defn (var_defn_field_index VarName) "" main_bod) main_bod in
    List.iteri (fun idx elem -> build_formula (varname, idx) formulas elem symbols) va.var_formulas in

  (* Creates a scope object and inserts the necessary instructions into main to populate the var_defns, and
   * into the function specified by builder to populate the scope object. *)
  let build_scope_obj
      fname (* The function name, or "globals" *)
      symbols (* The symbols to use when creating the functions *)
      vars (* The variables to build definitions and formula-functions for *)
      static_location_ptr (* The copy of the global pointer used in main *)
      var_defns_loc (* The copy of the global pointer used in the local function *)
      num_params (* How many parameters the function takes *)
      builder (* The LLVM builder for the local function *)
    =
    let cardinal = Llvm.const_int base_types.int_t (StringMap.cardinal vars) in
    let build_var_defns =
      let static_var_defns = Llvm.build_array_malloc base_types.var_defn_t cardinal (fname ^ "_static_var_defns") main_bod in
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
    let _ = Llvm.build_store (Llvm.const_int base_types.int_t 0) (Llvm.build_struct_gep scope_obj (scope_field_type_index ScopeRefCount) "" builder) builder in
    let paramarray = if num_params > 0 then Llvm.build_array_malloc base_types.value_p (Llvm.const_int base_types.int_t num_params) "paramarray" builder else Llvm.const_pointer_null (Llvm.pointer_type base_types.value_p) in
    let _ = Llvm.build_store paramarray (Llvm.build_struct_gep scope_obj (scope_field_type_index FunctionParams) "" builder) builder in
    let copy_fn_arg i =
      let param_addr = Llvm.build_in_bounds_gep paramarray [|Llvm.const_int base_types.int_t i|] (fname ^ "_param_" ^ string_of_int i ^ "_loc") builder in
      ignore (Llvm.build_store (Llvm.param (StringMap.find fname function_llvalues) i) param_addr builder) in
    List.iter copy_fn_arg (zero_until num_params);
    let _ = Llvm.build_call (Hashtbl.find runtime_functions "null_init") [|scope_obj|] "" builder in
    build_var_defns ; scope_obj in
  (* End of build_scope_obj *)

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

    let scope_obj = build_scope_obj fname symbols fn_def.func_body static_location_ptr var_defns_loc (List.length fn_def.func_params) builder in

    let ret = snd fn_def.func_ret_val in
    match ret with
      Id(name) ->
      (
        match (try StringMap.find name symbols with Not_found -> raise(LogicError("Something went wrong with your semantic analysis - " ^ name ^ " not found"))) with
          LocalVariable(i) ->
          let llvm_var = Llvm.build_call getVar [|scope_obj; Llvm.const_int base_types.int_t i|] "return_variable" builder in
          let llvm_retval = Llvm.build_call getVal [|llvm_var; Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "return_value" builder in
          ignore (Llvm.build_ret llvm_retval builder)
        | _ -> print_endline (string_of_expr ret); raise(TransformedAway("Error in " ^ fname ^ ": The return value should always have been transformed into a local variable"))
      )
    | _ -> print_endline (string_of_expr ret); raise(TransformedAway("Error in " ^ fname ^ ": The return value should always have been transformed into a local variable")) in
  (* End of build_function *)

  (* Build the global scope object *)
  let vardefn_p_p = Llvm.build_alloca base_types.var_defn_p "v_p_p" main_bod in
  let global_scope_obj = build_scope_obj "globals" global_symbols globals vardefn_p_p vardefn_p_p 0 main_bod in
  let _ = Llvm.build_call (Hashtbl.find runtime_functions "incStack") [||] "" main_bod in
  let _ = Llvm.build_store global_scope_obj global_scope_loc main_bod in

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
