
(* Extend code generator *)

type something = {
  range : Llvm.lltype;
  range_p : Llvm.lltype;
  subrange : Llvm.lltype;
  void : Llvm.lltype;
};;

let translate (globals, functions) =
  (*let build_struct ctx (name, tl) =
    let my_struct = Llvm.named_struct_type ctx name in
    let _ = Llvm.struct_set_body my_struct (Array.of_list tl) false in
    my_struct*)
  let context = Llvm.global_context () in
  let setup_types ctx =
    let range = Llvm.named_struct_type ctx "range"
    and subrange = Llvm.named_struct_type ctx "subrange" in
    let _ = Llvm.struct_set_body range (Array.of_list []) false
    and _ = Llvm.struct_set_body range (Array.of_list []) in
    {
      range = range;
      range_p = (Llvm.pointer_type range);
      subrange = subrange;
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
        let my_val = Llvm.build_alloca base_types.range "some_shit" builder in
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
