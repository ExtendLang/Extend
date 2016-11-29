
(* Extend code generator *)

exception NotImplemented

type something = {
  range_t : Llvm.lltype;
  subrange_t : Llvm.lltype;
  formula_t : Llvm.lltype;
  status_t : Llvm.lltype;
  value_t : Llvm.lltype;
  dimensions_t : Llvm.lltype;
  string_t : Llvm.lltype;
  number_t : Llvm.lltype;
  range_p : Llvm.lltype;
  subrange_p : Llvm.lltype;
  formula_p : Llvm.lltype;
  status_p : Llvm.lltype;
  value_p : Llvm.lltype;
  string_p : Llvm.lltype;
  string_p_p : Llvm.lltype;
  int_t : Llvm.lltype;
  long_t : Llvm.lltype;
  flags_t : Llvm.lltype;
  char_t : Llvm.lltype;
  bool_t : Llvm.lltype;
  void_t : Llvm.lltype;
  char_p : Llvm.lltype;
  char_p_p : Llvm.lltype;
  (*void_p : Llvm.lltype;*)
};;

let helper_functions = Hashtbl.create 10
let extern_functions = Hashtbl.create 10

type value_field_flags = Empty | Number | String | Range
let value_field_flags_index = function
    Empty -> 0
  | Number -> 1
  | String -> 2
  | Range -> 3

type value_field = Flags | Number | String | Subrange
let value_field_index = function
    Flags -> 0
  | Number -> 1
  | String -> 2
  | Subrange -> 3

type range_field = Rows | Columns | Values | Statuses | Formulas
let range_field_index = function
    Rows -> 0
  | Columns -> 1
  | Values -> 2
  | Statuses -> 3
  | Formulas -> 4

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

type string_field = StringCharPtr | StringLen | StringRefCount
let string_field_index = function
    StringCharPtr -> 0
  | StringLen -> 1
  | StringRefCount -> 2

let (=>) struct_ptr elem = (fun val_name builder ->
    let the_pointer = Llvm.build_struct_gep struct_ptr elem "the_pointer" builder in
    Llvm.build_load the_pointer val_name builder)

let create_extern_functions ctx bt the_module =
  let add_extern_func fname ftype returntype arglist =
    let the_func = Llvm.declare_function fname (ftype returntype (Array.of_list arglist)) the_module
    in Hashtbl.add extern_functions fname the_func in
  add_extern_func "strlen" Llvm.function_type bt.long_t [bt.char_p];
  add_extern_func "llvm.memcpy.p0i8.p0i8.i64" Llvm.function_type bt.void_t [bt.char_p; bt.char_p; bt.long_t; bt.int_t; bt.bool_t] ;
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

  let create_get_val fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.int_t [bt.range_p; bt.int_t; bt.int_t] in
    let _ = Llvm.build_ret (Llvm.const_int bt.int_t (-1)) fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  let create_deref_subrange fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.int_t [bt.subrange_p] in
    let the_base_range = ((Llvm.param fn_def 0) => (subrange_field_index BaseRangePtr))"the_base_range" fn_bod in
    let the_val = Llvm.build_call
        (Hashtbl.find helper_functions "get_val")
        (Array.of_list [the_base_range; (Llvm.const_int bt.int_t 0); (Llvm.const_int bt.int_t 0)])
        "the_contents" fn_bod in
    let _ = Llvm.build_ret the_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  let create_new_string fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.string_p [bt.char_p] in
    let the_string_ptr = Llvm.build_malloc bt.string_t "the_string_ptr" fn_bod in
    let src_char_ptr = Llvm.param fn_def 0 in
    let dst_char_ptr_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringCharPtr) "dst_char_ptr_ptr" fn_bod in
    let string_len = Llvm.build_call (Hashtbl.find extern_functions "strlen") [|src_char_ptr|] "string_len" fn_bod in
    let extra_byte = Llvm.build_add string_len (Llvm.const_int bt.long_t 1) "extra_byte" fn_bod in
    let strlen_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringLen) "strlen_ptr" fn_bod in
    let refcount_ptr = Llvm.build_struct_gep the_string_ptr (string_field_index StringRefCount) "refcount" fn_bod in
    let dst_char_ptr = Llvm.build_array_malloc bt.char_t extra_byte "dst_char_ptr" fn_bod in
    let _ = Llvm.build_store dst_char_ptr dst_char_ptr_ptr fn_bod in
    let _ = Llvm.build_call (Hashtbl.find extern_functions "llvm.memcpy.p0i8.p0i8.i64")
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
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  let create_box_value_number fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.value_p [bt.int_t] in
    let str = Llvm.param fn_def 0 in
    let ret_val = Llvm.build_malloc bt.value_t "" fn_bod in
    let sp = Llvm.build_struct_gep ret_val (value_field_index Number) "num_pointer" fn_bod in
    let _ = Llvm.build_store str sp fn_bod in
    let _ = Llvm.build_ret ret_val fn_bod in
    Hashtbl.add helper_functions fname fn_def in

  let create_box_single_value fname =
    let (fn_def, fn_bod) = create_def_bod fname bt.subrange_p [bt.value_p] in
    let value = Llvm.param fn_def 0 in
    let subrange = Llvm.build_malloc bt.subrange_t "" fn_bod in
    let range = Llvm.build_malloc bt.range_t "" fn_bod in
    let rp = Llvm.build_struct_gep subrange (subrange_field_index BaseRangePtr) "range_p" fn_bod in
    let vp = Llvm.build_struct_gep range (range_field_index Values) "value_p" fn_bod in
    let _ = Llvm.build_store value vp fn_bod in
    let _ = Llvm.build_store range rp fn_bod in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetCol) "" fn_bod) in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetRow) "" fn_bod) in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeRows) "" fn_bod) in
    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeCols) "" fn_bod) in
    let _ = Llvm.build_ret subrange fn_bod in
    Hashtbl.add helper_functions fname fn_def in

    create_is_subrange_1x1 "is_subrange_1x1";
    create_get_val "get_val";
    create_deref_subrange "deref_subrange";
    create_new_string "new_string";
    create_box_native_string_list "box_native_string_list";
    create_box_value_string "box_value_string";
    create_box_value_number "box_value_number";
    create_box_single_value "box_single_value";
    ()

let create_main fnames ctx bt the_module =
  let main_def = Llvm.define_function "main"
      (Llvm.function_type bt.int_t (Array.of_list [bt.int_t; bt.char_p_p]))
      the_module in
  let main_bod = Llvm.builder_at_end ctx (Llvm.entry_block main_def) in
  let inp = Llvm.build_alloca bt.subrange_t "input_arg" main_bod in
  (* Put input args in inp *)
  let _ = Llvm.build_call
         (
           let (a,b) = Ast.StringMap.find
               "main"
               fnames
           in
           b
         )
         (Array.of_list [inp])
         "" main_bod in
  let str_format_str = Llvm.build_global_stringptr "%s\n" "fmt" main_bod in
  let int_format_str = Llvm.build_global_stringptr "%d\n" "fmt" main_bod in
  let boxed_args = Llvm.build_call (Hashtbl.find helper_functions "box_native_string_list") [|(Llvm.param main_def 0);(Llvm.param main_def 1)|] "args" main_bod in
  let _ = Llvm.build_ret (Llvm.const_int bt.int_t 0) main_bod in
  ()

let translate (globals, functions, externs) =
  (*let build_struct ctx (name, tl) =
    let my_struct = Llvm.named_struct_type ctx name in
    let _ = Llvm.struct_set_body my_struct (Array.of_list tl) false in
    my_struct*)
  let context = Llvm.global_context () in
  let setup_types ctx =
    let range_t = Llvm.named_struct_type ctx "range" (*Range struct is a 2D Matrix of values*)
    and subrange_t = Llvm.named_struct_type ctx "subrange" (*Subrange is a wrapper around a range to cut cells*)
    and int_t = Llvm.i32_type ctx (*Integer*)
    and long_t = Llvm.i64_type ctx
    and flags_t = Llvm.i8_type ctx (*Flags for statuses*)
    and char_t = Llvm.i8_type ctx (*Simple ASCII character*)
    and bool_t = Llvm.i1_type ctx (*boolean 0 = false, 1 = true*)
    and void_t = Llvm.void_type ctx (**)
    and value_t = Llvm.named_struct_type ctx "value" (*Value encapsulates the content of a cell*)
    and dimensions_t = Llvm.named_struct_type ctx "dimensions" (**)
    and status_t = Llvm.named_struct_type ctx "status" (*Status indicates how a cell must be treated*)
    and formula_t = Llvm.named_struct_type ctx "formula" (*Formula is a hint on how to calculate the value of a cell*)
    and string_t = Llvm.named_struct_type ctx "string"
    and number_t = Llvm.i32_type ctx in
    let range_p = (Llvm.pointer_type range_t)
    and subrange_p = (Llvm.pointer_type subrange_t)
    and value_p = (Llvm.pointer_type value_t)
    and status_p = (Llvm.pointer_type status_t)
    and formula_p = (Llvm.pointer_type formula_t)
    and char_p = (Llvm.pointer_type char_t)
    and string_p = (Llvm.pointer_type string_t)
    and char_p_p = (Llvm.pointer_type (Llvm.pointer_type char_t))
    and string_p_p = (Llvm.pointer_type (Llvm.pointer_type string_t))
    (*and void_p = (Llvm.pointer_type void_t)*) in
    let _ = Llvm.struct_set_body range_t (Array.of_list [
        int_t(*rows*);
        int_t(*columns*);
        value_p(*2D array of cell values*);
        status_p(*2D array of calculation status for each cell*);
        formula_p(*List of formulas for a cell range*)
      ]) false
    and _ = Llvm.struct_set_body subrange_t (Array.of_list [
        range_p(*The target range*);
        int_t(*row offset*);
        int_t(*column offset*);
        int_t(*row count*);
        int_t(*column count*)
      ]) false
    and _ = Llvm.struct_set_body value_t (Array.of_list [
        flags_t (*First bit indicates whether it is an int or a range*);
        number_t (*Numeric value of the cell*);
        string_p (*String value of the cell if applicable*);
        subrange_p (*Range value of the cell if applicable*)
      ]) false
    and _ = Llvm.struct_set_body string_t (Array.of_list [
        char_p (*Pointer to null-terminated string*);
        long_t (*Length of string*);
        int_t (*Reference count*)
      ]) false
    and _ = Llvm.struct_set_body dimensions_t (Array.of_list [int_t; int_t]) false in
    {
      range_t = range_t;
      value_t = value_t;
      status_t = status_t;
      subrange_t = subrange_t;
      formula_t = formula_t;
      dimensions_t = dimensions_t;
      number_t = number_t;
      string_t = string_t;

      range_p = range_p;
      subrange_p = subrange_p;
      value_p = value_p;
      status_p = status_p;
      formula_p = formula_p;
      string_p = string_p;
      char_p = char_p;

      int_t = int_t;
      long_t = long_t;
      flags_t = flags_t;
      bool_t = bool_t;
      char_t = char_t;
      void_t = void_t;
      char_p_p = char_p_p;
      string_p_p = string_p_p;
      (*void_p = void_p;*)
    }
  and base_module = Llvm.create_module context "Extend" in
  let base_types = setup_types context in
  let build_externs =
    Ast.StringMap.fold
    (fun key (b: Ast.extern_func) a ->
      Ast.StringMap.add
      b.Ast.extern_fn_name
      (
        Llvm.declare_function
        b.Ast.extern_fn_name
        (
          Llvm.function_type base_types.subrange_p (Array.of_list (List.map (fun a -> base_types.subrange_p) b.Ast.extern_fn_params))
        )
        base_module
      )
      a
    )
    externs
    Ast.StringMap.empty
     in
  let build_function_names =
    Ast.StringMap.mapi (fun key (func: Ast.func_decl) ->
        (func, Llvm.define_function
           (if (key = "main") then "_main" else key)
           (Llvm.function_type base_types.subrange_p (Array.of_list (List.map (fun a -> base_types.subrange_p) func.Ast.func_params)))
           base_module)
      ) functions in
  let build_public_functions =
    Ast.StringMap.fold (fun k a b -> Ast.StringMap.add k a b) (Ast.StringMap.map (fun (b, c) -> c) build_function_names) build_externs in
  (* Declare the external functions that we need to call *)
  create_extern_functions context base_types base_module ;

  (* Define the internal helper functions we'll need to use *)
  create_helper_functions context base_types base_module ;

  (* Define the LLVM entry point for the program *)
  create_main build_function_names context base_types base_module ;

  let build_function_body =
    Ast.StringMap.iter (fun key (desc, func) ->
        let rec expr_eval expr scope builder ctx extern helpers bt =
          (*print_endline (Ast.string_of_expr expr);*)
          match expr with
            Ast.Precedence(a,b) -> expr_eval a scope builder ctx extern helpers bt; expr_eval b scope builder ctx extern helpers bt;
          | Ast.Call(fn,exl) ->
              let args = Array.of_list
                (List.rev (List.fold_left (fun a b -> (expr_eval b scope builder ctx extern helpers bt) :: a) [] exl)) in
              Llvm.build_call (
                Ast.StringMap.find fn build_public_functions
              ) args "" builder
          | Ast.LitString(str) ->
              let boxxx = Llvm.build_call
              (Hashtbl.find helpers "new_string")
              (Array.of_list [
                Llvm.build_global_stringptr str "glob_str" builder
              ]) "boxed_str" builder in
              let boxx = Llvm.build_call
              (Hashtbl.find helpers "box_value_string")
              (Array.of_list [boxxx]) "box_value_str" builder in
              let box = Llvm.build_call
              (Hashtbl.find helpers "box_single_value")
              [|boxx|] "box_subrange" builder
              in box
          | Ast.LitInt(i) -> let boxx = Llvm.build_call
              (Hashtbl.find helpers "box_value_number")
              (Array.of_list [Llvm.const_int bt.int_t i]) "box_value_str" builder in
              let box = Llvm.build_call
              (Hashtbl.find helpers "box_single_value")
              [|boxx|] "box_subrange" builder
              in box
          | Ast.BinOp(ex1,op,ex2) ->
              let val1 = (expr_eval ex1 scope builder ctx extern helpers bt)
              and val2 = (expr_eval ex2 scope builder ctx extern helpers bt) in
              (match op with
                Ast.Plus -> Llvm.build_add val1 val2 "" builder
              | Ast.Minus -> Llvm.build_sub val1 val2 "" builder
              | _ -> raise NotImplemented)
          | Ast.Id(name) -> Llvm.const_string ctx name
          | Ast.UnOp(op,expr) -> (match op with
                Ast.SizeOf -> let subrange = (expr_eval expr scope builder ctx extern helpers bt) in
                    let rows = Llvm.const_int bt.int_t 1 and cols = Llvm.const_int bt.int_t 1 in
                    let row_val = Llvm.build_array_malloc bt.value_t (Llvm.const_int bt.int_t 2) "" builder in
                    let sp = Llvm.build_struct_gep row_val (value_field_index Number) "" builder in
                    let _ = Llvm.build_store rows sp builder in
                    let col_val = Llvm.build_in_bounds_gep row_val [|Llvm.const_int bt.int_t 1|] "" builder in
                    let sp = Llvm.build_struct_gep col_val (value_field_index Number) "" builder in
                    let _ = Llvm.build_store cols sp builder in
                    let subrange = Llvm.build_malloc bt.subrange_t "" builder in
                    let range = Llvm.build_malloc bt.range_t "" builder in
                    let rp = Llvm.build_struct_gep subrange (subrange_field_index BaseRangePtr) "range_p" builder in
                    let vp = Llvm.build_struct_gep range (range_field_index Values) "value_p" builder in
                    let _ = Llvm.build_store row_val vp builder in
                    let _ = Llvm.build_store range rp builder in
                    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetCol) "" builder) in
                    let _ = Llvm.build_store (Llvm.const_int bt.int_t 0) (Llvm.build_struct_gep subrange (subrange_field_index BaseOffsetRow) "" builder) in
                    let _ = Llvm.build_store (Llvm.const_int bt.int_t 1) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeRows) "" builder) in
                    let _ = Llvm.build_store (Llvm.const_int bt.int_t 2) (Llvm.build_struct_gep subrange (subrange_field_index SubrangeCols) "" builder) in
                    subrange
              | _ -> raise NotImplemented)
          | Ast.Selection(expr, sel) -> expr_eval expr scope builder ctx extern helpers bt
          | _ -> print_endline (Ast.string_of_expr expr);raise NotImplemented in
        let builder = Llvm.builder_at_end context (Llvm.entry_block func) in
        let scope = Ast.StringMap.fold (
            fun a b c -> base_types.range_p :: c
          ) desc.Ast.func_body [] in
        let struct_f = Llvm.struct_type context (Array.of_list scope) in
        let struct_r = Llvm.build_malloc struct_f "_scope" builder in
        let _ = Ast.StringMap.fold (
            fun a b c ->
              List.fold_left (fun a b ->
                expr_eval b.Ast.formula_expr struct_r builder context extern_functions helper_functions base_types; a
              ) () b.Ast.var_formulas
            ; c + 1
          ) desc.Ast.func_body 0 in
        let (dims, expr) = desc.Ast.func_ret_val in
        let ret_v = expr_eval expr struct_r builder context extern_functions helper_functions base_types in
        Llvm.build_ret ret_v builder; ()
      ) build_function_names in
    base_module

let build_this ast_mapped =
  let modu = (translate ast_mapped) in
  let res = Llvm_analysis.assert_valid_module modu in
  modu
