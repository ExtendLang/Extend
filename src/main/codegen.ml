
(* Extend code generator *)

type something = {
  range_t : Llvm.lltype;
  subrange_t : Llvm.lltype;
  formula_t : Llvm.lltype;
  status_t : Llvm.lltype;
  value_t : Llvm.lltype;
  dimensions_t : Llvm.lltype;
  number_t : Llvm.lltype;
  range_p : Llvm.lltype;
  subrange_p : Llvm.lltype;
  formula_p : Llvm.lltype;
  status_p : Llvm.lltype;
  value_p : Llvm.lltype;
  int_t : Llvm.lltype;
  flags_t : Llvm.lltype;
  char_t : Llvm.lltype;
  bool_t : Llvm.lltype;
  void_t : Llvm.lltype;
  (*void_p : Llvm.lltype;*)
};;

let helper_functions = Hashtbl.create 10

type subrange_field = BaseRangePtr | BaseOffsetRow | BaseOffsetCol | SubrangeRows | SubrangeCols
let subrange_field_index = function
    BaseRangePtr -> 0
  | BaseOffsetRow -> 1
  | BaseOffsetCol -> 2
  | SubrangeRows -> 3
  | SubrangeCols -> 4

type dimensions_field = DimensionRows | DimensionCols
let dimensions_field_index = function
    DimensionRows -> 0
  | DimensionCols -> 1

let create_is_subrange_1x1 fname ctx bt the_module =
  let is_index_one fn builder idx =
    let the_pointer = Llvm.build_struct_gep (Llvm.param fn 0) (subrange_field_index idx) "the_pointer" builder in
    let the_value = Llvm.build_load the_pointer "the_value" builder in
    let the_bool = Llvm.build_icmp Llvm.Icmp.Eq the_value (Llvm.const_int bt.int_t 1) "the_bool" builder in
    the_bool in
  let fn_def = Llvm.define_function fname (Llvm.function_type bt.bool_t (Array.of_list [bt.subrange_p])) the_module in
  let fn_bod = Llvm.builder_at_end ctx (Llvm.entry_block fn_def) in
  let one_row = is_index_one fn_def fn_bod SubrangeRows in
  let one_col = is_index_one fn_def fn_bod SubrangeCols in
  let one_by_one = Llvm.build_and one_row one_col "one_by_one" fn_bod in
  let _ = Llvm.build_ret one_by_one fn_bod in
  Hashtbl.add helper_functions fname fn_def

let create_get_val fname ctx bt the_module =
  let fn_def = Llvm.define_function fname (Llvm.function_type bt.int_t (Array.of_list [bt.range_p; bt.int_t; bt.int_t])) the_module in
  let fn_bod = Llvm.builder_at_end ctx (Llvm.entry_block fn_def) in
  let _ = Llvm.build_ret (Llvm.const_int bt.int_t (-1)) fn_bod in
  Hashtbl.add helper_functions fname fn_def

let create_deref_subrange fname ctx bt the_module =
  let fn_def = Llvm.define_function fname (Llvm.function_type bt.int_t (Array.of_list [bt.subrange_p])) the_module in
  let fn_bod = Llvm.builder_at_end ctx (Llvm.entry_block fn_def) in
  let the_base_range_ptr = Llvm.build_struct_gep (Llvm.param fn_def 0) (subrange_field_index BaseRangePtr) "the_base_range_ptr" fn_bod in
  let the_base_range = Llvm.build_load the_base_range_ptr "the_base_range" fn_bod in
  let the_val = Llvm.build_call
      (Hashtbl.find helper_functions "get_val")
      (Array.of_list [the_base_range; (Llvm.const_int bt.int_t 0); (Llvm.const_int bt.int_t 0)])
      "the_contents" fn_bod in
  let _ = Llvm.build_ret the_val fn_bod in
  Hashtbl.add helper_functions fname fn_def

let translate (globals, functions) =
  (*let build_struct ctx (name, tl) =
    let my_struct = Llvm.named_struct_type ctx name in
    let _ = Llvm.struct_set_body my_struct (Array.of_list tl) false in
    my_struct*)
  let context = Llvm.global_context () in
  let setup_types ctx =
    let range_t = Llvm.named_struct_type ctx "range"
    and subrange_t = Llvm.named_struct_type ctx "subrange"
    and int_t = Llvm.i32_type ctx
    and flags_t = Llvm.i8_type ctx
    and char_t = Llvm.i8_type ctx
    and bool_t = Llvm.i1_type ctx
    and void_t = Llvm.void_type ctx
    and value_t = Llvm.named_struct_type ctx "value"
    and dimensions_t = Llvm.named_struct_type ctx "dimensions"
    and status_t = Llvm.named_struct_type ctx "status"
    and number_t = Llvm.i32_type ctx
    and formula_t = Llvm.named_struct_type ctx "formula" in
    let range_p = (Llvm.pointer_type range_t)
    and subrange_p = (Llvm.pointer_type subrange_t)
    and value_p = (Llvm.pointer_type value_t)
    and status_p = (Llvm.pointer_type status_t)
    and formula_p = (Llvm.pointer_type formula_t)
    (*and void_p = (Llvm.pointer_type void_t)*) in
    let _ = Llvm.struct_set_body range_t (Array.of_list [int_t; int_t; value_p; status_p; formula_p]) false
    and _ = Llvm.struct_set_body subrange_t (Array.of_list [range_p; int_t; int_t; int_t; int_t]) false
    and _ = Llvm.struct_set_body value_t (Array.of_list [flags_t; number_t]) false
    and _ = Llvm.struct_set_body dimensions_t (Array.of_list [int_t; int_t]) false in
    {
      range_t = range_t;
      value_t = value_t;
      status_t = status_t;
      subrange_t = subrange_t;
      formula_t = formula_t;
      dimensions_t = dimensions_t;
      number_t = number_t;

      range_p = range_p;
      subrange_p = subrange_p;
      value_p = value_p;
      status_p = status_p;
      formula_p = formula_p;

      int_t = int_t;
      flags_t = flags_t;
      bool_t = bool_t;
      char_t = char_t;
      void_t = void_t;
      (*void_p = void_p;*)
    }
  and base_module = Llvm.create_module context "Extend" in
  let base_types = setup_types context in
  let build_function_names =
    Ast.StringMap.mapi (fun key (func: Ast.func_decl) ->
        (func, Llvm.define_function
           (if (key = "main") then "_main" else key)
           (Llvm.function_type base_types.subrange_p (Array.of_list (List.map (fun a -> base_types.subrange_p) func.Ast.func_params)))
           base_module)
      ) functions in
  let main_def = Llvm.define_function "main" (Llvm.function_type base_types.int_t (Array.of_list [])) base_module in
  let main_bod = Llvm.builder_at_end context (Llvm.entry_block main_def) in
  let inp = Llvm.build_alloca base_types.subrange_t "input_arg" main_bod in
  (* Put input args in inp *)
  let _ = Llvm.build_call
         (
           let (a,b) = Ast.StringMap.find
               "main"
               build_function_names
           in
           b
         )
         (Array.of_list [inp])
         "" main_bod in
  let _ = Llvm.build_ret (Llvm.const_int base_types.int_t 0) main_bod in
  create_is_subrange_1x1 "is_subrange_1x1" context base_types base_module;
  create_get_val "get_val" context base_types base_module;
  create_deref_subrange "deref_subrange" context base_types base_module;
  let build_function_body =
    Ast.StringMap.iter (fun key (desc, func) ->
        let builder = Llvm.builder_at_end context (Llvm.entry_block func) in
        let scope = Ast.StringMap.fold (
            fun a b c -> base_types.range_p :: c
          ) desc.Ast.func_body [] in
        let struct_f = Llvm.struct_type context (Array.of_list scope) in
        let struct_r = Llvm.build_malloc struct_f "_scope" builder in
        let _ = Ast.StringMap.fold (
            fun a b c -> (*Llvm.build_struct_gep c struct_r builder;*) c + 1
          ) desc.Ast.func_body 0 in
        let res = Llvm.build_malloc base_types.subrange_t "ret" builder in
        Llvm.build_ret res builder; ()
      ) build_function_names in
    base_module

let build_this ast_mapped =
  let modu = (translate ast_mapped) in
  let res = Llvm_analysis.assert_valid_module modu in
  modu
