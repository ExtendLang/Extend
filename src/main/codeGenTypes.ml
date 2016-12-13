type something = {
  var_instance_t : Llvm.lltype;
  subrange_t : Llvm.lltype;
  resolved_formula_t : Llvm.lltype;
  status_t : Llvm.lltype;
  value_t : Llvm.lltype;
  dimensions_t : Llvm.lltype;
  var_defn_t : Llvm.lltype;
  var_defn_p : Llvm.lltype;
  string_t : Llvm.lltype;
  number_t : Llvm.lltype;
  extend_scope_t : Llvm.lltype;
  formula_t : Llvm.lltype;
  formula_call_t : Llvm.lltype;
  formula_p : Llvm.lltype;
  formula_call_p : Llvm.lltype;
  var_instance_p : Llvm.lltype;
  subrange_p : Llvm.lltype;
  resolved_formula_p : Llvm.lltype;
  status_p : Llvm.lltype;
  value_p : Llvm.lltype;
  extend_scope_p : Llvm.lltype;
  string_p : Llvm.lltype;
  string_p_p : Llvm.lltype;
  var_instance_p_p : Llvm.lltype;
  int_t : Llvm.lltype;
  long_t : Llvm.lltype;
  flags_t : Llvm.lltype;
  char_t : Llvm.lltype;
  bool_t : Llvm.lltype;
  void_t : Llvm.lltype;
  char_p : Llvm.lltype;
  char_p_p : Llvm.lltype;
  (*void_p : Llvm.lltype;*)
  float_t : Llvm.lltype;
};;

type scope_field_type = VarDefn | VarInst | VarNum | ScopeRefCount | FunctionParams
let scope_field_type_index = function
    VarDefn -> 0
  | VarInst -> 1
  | VarNum -> 2
  | ScopeRefCount -> 3
  | FunctionParams -> 4

type value_field_flags = Empty | Number | String | Range
let value_field_flags_index = function
    Empty -> 0
  | Number -> 1
  | String -> 2
  | Range -> 3
let int_to_type_array = [|"Empty"; "Number"; "String"; "Range"|]

type value_field = Flags | Number | String | Subrange
let value_field_index = function
    Flags -> 0
  | Number -> 1
  | String -> 2
  | Subrange -> 3

type var_defn_field = Rows | Cols | NumFormulas | Formulas | OneByOne | VarName
let var_defn_field_index = function
    Rows -> 0
  | Cols -> 1
  | NumFormulas -> 2
  | Formulas -> 3
  | OneByOne -> 4
  | VarName -> 5

type formula_field  = FromFirstRow | RowStartNum | ToLastRow | RowEndNum | FromFirstCols | ColStartNum | ToLastCol | ColEndNum | IsSingleRow | IsSingleCol | FormulaCall
let formula_field_index = function
    FromFirstRow -> 0
  | RowStartNum -> 1
  | ToLastRow -> 2
  | RowEndNum -> 3
  | FromFirstCols -> 4
  | ColStartNum -> 5
  | ToLastCol -> 6
  | ColEndNum -> 7
  | IsSingleRow -> 8
  | IsSingleCol -> 9
  | FormulaCall -> 10

type var_instance_field = Rows | Cols | NumFormulas | Formulas | Closure | Values | Status
let var_instance_field_index = function
    Rows -> 0
  | Cols -> 1
  | NumFormulas -> 2
  | Formulas -> 3
  | Closure -> 4
  | Values -> 5
  | Status -> 6

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

let setup_types ctx =
  let var_instance_t = Llvm.named_struct_type ctx "var_instance" (*Range struct is a 2D Matrix of values*)
  and subrange_t = Llvm.named_struct_type ctx "subrange" (*Subrange is a wrapper around a range to cut cells*)
  and int_t = Llvm.i32_type ctx (*Integer*)
  and long_t = Llvm.i64_type ctx
  and float_t = Llvm.double_type ctx
  and flags_t = Llvm.i8_type ctx (*Flags for statuses*)
  and char_t = Llvm.i8_type ctx (*Simple ASCII character*)
  and bool_t = Llvm.i1_type ctx (*boolean 0 = false, 1 = true*)
  and void_t = Llvm.void_type ctx (**)
  and value_t = Llvm.named_struct_type ctx "value" (*Value encapsulates the content of a cell*)
  and dimensions_t = Llvm.named_struct_type ctx "dimensions" (**)
  and status_t = Llvm.named_struct_type ctx "status" (*Status indicates how a cell must be treated*)
  and resolved_formula_t = Llvm.named_struct_type ctx "resolved_formula"
  and extend_scope_t = Llvm.named_struct_type ctx "extend_scope"
  and var_defn_t = Llvm.named_struct_type ctx "var_def"
  and formula_t = Llvm.named_struct_type ctx "formula"
  and string_t = Llvm.named_struct_type ctx "string" in
  let var_instance_p = (Llvm.pointer_type var_instance_t)
  and var_defn_p = Llvm.pointer_type var_defn_t
  and resolved_formula_p = (Llvm.pointer_type resolved_formula_t)
  and subrange_p = (Llvm.pointer_type subrange_t)
  and value_p = (Llvm.pointer_type value_t)
  and status_p = (Llvm.pointer_type status_t)
  and extend_scope_p = (Llvm.pointer_type extend_scope_t)
  and char_p = (Llvm.pointer_type char_t)
  and string_p = (Llvm.pointer_type string_t)
  and char_p_p = (Llvm.pointer_type (Llvm.pointer_type char_t))
  and string_p_p = (Llvm.pointer_type (Llvm.pointer_type string_t))
  and number_t = float_t
  and formula_p = (Llvm.pointer_type formula_t)
  (*and void_p = (Llvm.pointer_type void_t)*) in
  let var_instance_p_p = (Llvm.pointer_type var_instance_p)
  and formula_call_t = (Llvm.function_type value_p [|extend_scope_p(*scope*); int_t(*row*); int_t(*col*)|]) in
  let formula_call_p = Llvm.pointer_type formula_call_t in
  let _ = Llvm.struct_set_body var_instance_t (Array.of_list [
      int_t(*rows*);
      int_t(*columns*);
      int_t(*numFormulas*);
      resolved_formula_p(*formula with resolved dimensions*);
      extend_scope_p(*scope that contains all variables of a function*);
      value_p(*2D array of cell values*);
      status_p(*2D array of calculation status for each cell*);
      char_p(*Name*);
    ]) false
  and _ = Llvm.struct_set_body var_defn_t (Array.of_list [
      int_t(*Rows*);
      int_t(*Cols*);
      int_t(*Number of formulas*);
      formula_p;
      char_t(*Is one by one range*);
      char_p(*Name*);
    ]) false
  and _ = Llvm.struct_set_body formula_t (Array.of_list [
      char_t (*from First row*);
      int_t (*row Start num*);
      char_t (*to last row*);
      int_t (*row end num*);
      char_t (*from first col*);
      int_t (*col start*);
      char_t (*to last col*);
      int_t (*col end num*);
      char_t (* is single row *);
      char_t (* is single col *);
      formula_call_p (*formula to call*);
    ]) false
  and _ = Llvm.struct_set_body extend_scope_t (Array.of_list [
      var_defn_p(*variable definitions*);
      var_instance_p_p(*variable instances*);
      int_t(*number of variables*);
      int_t(*reference count*);
      Llvm.pointer_type value_p;
    ]) false
  and _ = Llvm.struct_set_body subrange_t (Array.of_list [
      var_instance_p(*The target range*);
      int_t(*row offset*);
      int_t(*column offset*);
      int_t(*row count*);
      int_t(*column count*)
    ]) false
  and _ = Llvm.struct_set_body value_t (Array.of_list [
      flags_t (*First bit indicates whether it is an int or a range*);
      number_t (*Numeric value of the cell*);
      string_p (*String value of the cell if applicable*);
      subrange_p (*Range value of the cell if applicable*);
      (*float_t (Double value of the cell*)
    ]) false
  and _ = Llvm.struct_set_body string_t (Array.of_list [
      char_p (*Pointer to null-terminated string*);
      long_t (*Length of string*);
      int_t (*Reference count*)
    ]) false
  and _ = Llvm.struct_set_body dimensions_t (Array.of_list [int_t; int_t]) false in
  {
    var_instance_t = var_instance_t;
    value_t = value_t;
    status_t = status_t;
    subrange_t = subrange_t;
    resolved_formula_t = resolved_formula_t;
    dimensions_t = dimensions_t;
    number_t = number_t;
    string_t = string_t;
    extend_scope_t = extend_scope_t;
    formula_t = formula_t;
    formula_call_t = formula_call_t;

    var_defn_t = var_defn_t;
    var_defn_p = var_defn_p;
    var_instance_p = var_instance_p;
    subrange_p = subrange_p;
    value_p = value_p;
    status_p = status_p;
    resolved_formula_p = resolved_formula_p;
    string_p = string_p;
    char_p = char_p;
    extend_scope_p = extend_scope_p;
    formula_p = formula_p;
    formula_call_p = formula_call_p;

    var_instance_p_p = var_instance_p_p;

    int_t = int_t;
    long_t = long_t;
    float_t = float_t;
    flags_t = flags_t;
    bool_t = bool_t;
    char_t = char_t;
    void_t = void_t;
    char_p_p = char_p_p;
    string_p_p = string_p_p;
    (*void_p = void_p;*)
  }
