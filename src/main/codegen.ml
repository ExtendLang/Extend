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
  add_runtime_func "llvm.memcpy.p0i8.p0i8.i64" bt.void_t [|bt.char_p; bt.char_p; bt.long_t; bt.int_t; bt.bool_t|] ;
  add_runtime_func "getVal" bt.value_p [|bt.var_instance_p; bt.int_t; bt.int_t|] ;
  add_runtime_func "clone_value" bt.value_p [|bt.value_p;|] ;
  (* add_runtime_func "freeMe" (Llvm.void_type ctx) [|bt.extend_scope_p;|] ; *)
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
  let array_of_vardefn_ptrs = Llvm.define_global "array_of_vardefn_ptrs" (Llvm.const_array base_types.var_defn_p vardefn_array) base_module in

  (* Create the pointer to the global scope object *)
  let global_scope_loc = Llvm.define_global "global_scope_loc" (Llvm.const_pointer_null base_types.extend_scope_p) base_module in

  let main_def = Llvm.define_function "main" (Llvm.function_type base_types.int_t [|base_types.int_t; base_types.char_p_p|]) base_module in
  let main_bod = Llvm.builder_at_end context (Llvm.entry_block main_def) in

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
    let store_number value_ptr store_builder number_llvalue =
      let sp = Llvm.build_struct_gep value_ptr (value_field_index Number) "num_pointer" store_builder in
      let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep value_ptr (value_field_index Flags) "" store_builder) store_builder in
      ignore (Llvm.build_store number_llvalue sp store_builder) in
    let store_empty value_ptr store_builder =
      ignore (Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Empty)) (Llvm.build_struct_gep value_ptr (value_field_index Flags) "" store_builder) store_builder) in
    let rec build_expr old_builder exp = match exp with
        LitInt(i) -> let vvv = Llvm.const_float base_types.float_t (float_of_int i) in
        let ret_val = Llvm.build_malloc base_types.value_t "" old_builder in
        let _ = store_number ret_val old_builder vvv in
        (ret_val, old_builder)
      | LitFlt(f) -> let vvv = Llvm.const_float base_types.float_t f in
        let ret_val = Llvm.build_malloc base_types.value_t "" old_builder in
        let _ = store_number ret_val old_builder vvv in
        (ret_val, old_builder)
      | Empty ->
        let ret_val = Llvm.build_malloc base_types.value_t "" old_builder in
        let _ = store_empty ret_val old_builder in
        (ret_val, old_builder)
      | Id(name) ->
        (
          match (try StringMap.find name symbols with Not_found -> raise(LogicError("Something went wrong with your semantic analysis - " ^ name ^ " not found"))) with
            LocalVariable(i) ->
            let llvm_var = Llvm.build_call getVar [|local_scope; Llvm.const_int base_types.int_t i|] "" old_builder in
            (Llvm.build_call getVal [|llvm_var; Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "" old_builder, old_builder)
          | GlobalVariable(i) ->
            let llvm_var = Llvm.build_call getVar [|global_scope; Llvm.const_int base_types.int_t i|] "" old_builder in
            (Llvm.build_call getVal [|llvm_var; Llvm.const_int base_types.int_t 0; Llvm.const_int base_types.int_t 0|] "" old_builder, old_builder)
          | FunctionParameter(i) ->
            let paramarray = (local_scope => (scope_field_type_index FunctionParams)) "paramarray" old_builder in
            let param_addr = Llvm.build_in_bounds_gep paramarray [|Llvm.const_int base_types.int_t i|] "param_addr" old_builder in
            let param = Llvm.build_load param_addr "param" old_builder in
            (Llvm.build_call (Hashtbl.find runtime_functions "clone_value") [|param|] "" old_builder, old_builder)
          | ExtendFunction(i) -> raise(LogicError("Something went wrong with your semantic analyis - function " ^ name ^ " used as variable in RHS for " ^ varname))
        )
      | Selection(expr, sel) -> build_expr old_builder expr
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
        ) args "" call_builder in
        (result, call_builder)
      | UnOp(SizeOf,expr) -> let vvv = Llvm.const_float base_types.float_t 0.0 in
        let ret_val = Llvm.build_malloc base_types.value_t "" old_builder in
        let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" old_builder in
        let _ = Llvm.build_store (Llvm.const_int base_types.char_t (value_field_flags_index Number)) (Llvm.build_struct_gep ret_val (value_field_index Flags) "" old_builder) old_builder in
        let _ = Llvm.build_store vvv sp old_builder in
        (ret_val, old_builder)
      | UnOp(Truthy, expr) ->
        let ret_val = Llvm.build_malloc base_types.value_t "" old_builder in
        let (expr_val, expr_builder) = build_expr old_builder expr in
        let merge_bb = Llvm.append_block context "merge" form_decl in
        let merge_builder = Llvm.builder_at_end context merge_bb in

        let truthy_bb = Llvm.append_block context "truthy" form_decl in
        let truthy_builder = Llvm.builder_at_end context truthy_bb in
        let _ = store_number ret_val truthy_builder (Llvm.const_float base_types.float_t 1.0) in
        let _ = Llvm.build_br merge_bb truthy_builder in

        let falsey_bb = Llvm.append_block context "falsey" form_decl in
        let falsey_builder = Llvm.builder_at_end context falsey_bb in
        let _ = store_number ret_val falsey_builder (Llvm.const_float base_types.float_t 0.0) in
        let _ = Llvm.build_br merge_bb falsey_builder in

        let empty_bb = Llvm.append_block context "empty" form_decl in
        let empty_builder = Llvm.builder_at_end context empty_bb in
        let _ = store_empty ret_val empty_builder in
        let _ = Llvm.build_br merge_bb empty_builder in

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
      | ReducedTernary(cond_var, true_var, false_var) ->
        let ret_val_addr = Llvm.build_alloca base_types.value_p "" old_builder in
        let (cond_val, _) = build_expr old_builder (Id(cond_var)) in (* Relying here on the fact that Id() doesn't change the builder *)
        let merge_bb = Llvm.append_block context "merge" form_decl in
        let merge_builder = Llvm.builder_at_end context merge_bb in
        let ret_val = Llvm.build_load ret_val_addr "ret_val" merge_builder in

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
        let ret_val_empty = Llvm.build_malloc base_types.value_t "" empty_builder in
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

      | UnOp( _, expr) -> print_endline (Ast.string_of_expr exp); raise NotImplemented
      | unknown_expr -> print_endline (string_of_expr unknown_expr);raise NotImplemented in
    let (ret_value_p, final_builder) = build_expr builder_at_top formula_expr in
    let _ = Llvm.build_ret ret_value_p final_builder in
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
                         | _ -> print_endline "Absdim"; raise NotImplemented
            ) intDim builder
          )
        | Some(Rel(e)) -> (
            ignore (Llvm.build_store (Llvm.const_int base_types.bool_t 0) boolAll builder);
            Llvm.build_store (
              match e with LitInt(i) -> Llvm.const_int base_types.int_t i
              | _ -> print_endline "Reldim"; raise NotImplemented
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
        DimId(a) -> (match StringMap.find a symbols with LocalVariable(i) -> i | GlobalVariable(i) -> i | _ -> raise(TransformedAway("Error in " ^ varname ^ ": The LHS expresssions should always either have dimension length 1 or be the name of a variable in their own scope.")))
      | DimInt(1) -> 1
      | DimInt(_) -> print_endline "Non1Dim" ; raise(NotImplemented) in
    let _ = (match va.var_rows with
          DimInt(1) -> Llvm.build_store (Llvm.const_int base_types.bool_t 1) (Llvm.build_struct_gep defn (var_defn_field_index OneByOne) "" main_bod) main_bod
        | DimInt(_) -> print_endline "Non1Dim" ; raise(NotImplemented)
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
