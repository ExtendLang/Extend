
(* Extend code generator *)

type something = {
  range_t : Llvm.lltype;
  subrange_t : Llvm.lltype;
  formula_t : Llvm.lltype;
  status_t : Llvm.lltype;
  value_t : Llvm.lltype;
  range_p : Llvm.lltype;
  subrange_p : Llvm.lltype;
  formula_p : Llvm.lltype;
  status_p : Llvm.lltype;
  value_p : Llvm.lltype;
  int_t : Llvm.lltype;
  void : Llvm.lltype;
};;

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
    and value_t = Llvm.named_struct_type ctx "value"
    and status_t = Llvm.named_struct_type ctx "status"
    and formula_t = Llvm.named_struct_type ctx "formula" in
    let range_p = (Llvm.pointer_type range_t)
    and subrange_p = (Llvm.pointer_type subrange_t)
    and value_p = (Llvm.pointer_type value_t)
    and status_p = (Llvm.pointer_type status_t)
    and formula_p = (Llvm.pointer_type formula_t) in
    let _ = Llvm.struct_set_body range_t (Array.of_list [int_t; int_t; value_p; status_p; formula_p]) false
    and _ = Llvm.struct_set_body subrange_t (Array.of_list [range_p; int_t; int_t; int_t; int_t]) false in
    {
      range_t = range_t;
      value_t = value_t;
      status_t = status_t;
      subrange_t = subrange_t;
      formula_t = formula_t;

      range_p = range_p;
      subrange_p = subrange_p;
      value_p = value_p;
      status_p = status_p;
      formula_p = formula_p;

      int_t = int_t;
      void = (Llvm.void_type ctx);
    }
  and base_module = Llvm.create_module context "Extend" in
  let base_types = setup_types context in
  let build_function_names =
    Ast.StringMap.mapi (fun key func ->
        (func, Llvm.define_function key (Llvm.function_type base_types.range_p (Array.of_list [])) base_module)
      ) functions in
  let build_function_body =
    Ast.StringMap.iter (fun key (desc, func) ->
        let builder = Llvm.builder_at_end context (Llvm.entry_block func) in
        let my_val = Llvm.build_alloca base_types.range_t "some_shit" builder in
        Llvm.build_ret my_val builder; () (*Llvm.build_ret_void builder; ()*)
      ) build_function_names in
    base_module

let build_this input =
  let ast_raw = Parser.program Scanner.token input in
  let ast_imp_res = Transform.load_imports (Transform.expand_imports ast_raw) in
  let ast_expanded = Transform.expand_expressions ast_imp_res in
  let ast_mapped = Transform.create_maps ast_expanded in
  let modu = (translate ast_mapped) in
  let res = Llvm_analysis.assert_valid_module modu in
  modu
