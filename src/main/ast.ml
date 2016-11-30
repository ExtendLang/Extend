type op       = Plus | Minus | Times | Divide | Mod | Pow |
                LShift | RShift | BitOr | BitAnd | BitXor |
                Eq | NotEq | Gt | Lt | GtEq | LtEq | LogAnd | LogOr
type unop     = Neg | LogNot | BitNot | SizeOf | TypeOf | Row | Column | Truthy

type expr     = LitInt of int |
                LitFlt of float |
                LitString of string |
                LitRange of (expr list) list |
                Id of string |
                Empty |
                Wild |
                BinOp of expr * op * expr |
                UnOp of unop * expr |
                Ternary of expr * expr * expr |
                Switch of expr option * case list * expr |
                Call of string * expr list |
                Selection of expr * sel |
                Precedence of expr * expr
and  index    = Abs of expr |
                Rel of expr |
                DimensionStart |
                DimensionEnd
and  slice    = index option * index option
and  sel      = slice option * slice option
and  case     = expr list * expr

type dim      = expr option * expr option
type var      = dim * string
type assign   = string * sel * expr option
type init     = string * expr option
type stmt     = Assign of assign |
                Varinit of dim * init list

type raw_func = {
    name: string;
    params: var list;
    body: stmt list;
    raw_asserts: expr list;
    ret_val: dim * expr;
}

type extern_func = {
    extern_fn_name: string;
    extern_fn_params: var list;
    extern_fn_libname: string;
    extern_ret_val: dim;
}

type library  = Library of string * extern_func list
type raw_program = string list * stmt list * raw_func list * library list

(* Desugared types below *)
module StringMap = Map.Make(String)
type formula  = {
  formula_row_start: index;
  formula_row_end: index option;
  formula_col_start: index;
  formula_col_end: index option;
  formula_expr: expr;
}

type dim_expr = DimInt of int
              | DimId of string

type variable = {
  var_rows: dim_expr;
  var_cols: dim_expr;
  var_formulas: formula list;
}

type func_decl = {
  func_params: var list;
  func_body: variable StringMap.t;
  func_asserts: expr list;
  func_ret_val: dim * expr;
}

type program = (variable StringMap.t) * (func_decl StringMap.t) * (extern_func StringMap.t)

type listable = Inits of init list |
                Vars of var list |
                Stmts of stmt list |
                RawFuncs of raw_func list |
                Externs of extern_func list |
                Libraries of library list |
                Exprs of expr list |
                Rows of (expr list) list |
                Strings of string list |
                Cases of case list |
                Formulas of formula list

exception IllegalRangeLiteral of string
exception TransformedAway of string

let quote_string str =
  let escape_characters = Str.regexp "[\n \t \r \\ \"]" in
  let replace_fn s = match Str.matched_string s with
    "\n" -> "\\n"   |
    "\t" -> "\\t"   |
    "\r" -> "\\r"   |
    "\\" -> "\\\\"  |
    "\"" -> "\\\""  |
    _    -> Str.matched_string s in
  "\"" ^ Str.global_substitute escape_characters replace_fn str ^ "\""

let string_of_op o = "\"" ^ (match o with
    Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/" | Mod -> "%" | Pow -> "**" |
    LShift -> "<<" | RShift -> ">>" | BitOr -> "|" | BitAnd -> "&" | BitXor -> "^" |
    Eq -> "==" | NotEq -> "!-" | Gt -> ">" | Lt -> "<" | GtEq -> ">=" | LtEq -> "<=" |
    LogAnd -> "&& " | LogOr -> "||" ) ^ "\""

let string_of_unop = function
    Neg -> "\"-\"" | LogNot -> "\"!\"" | BitNot -> "\"~\"" | Truthy -> "\"truthy\"" |
    SizeOf -> "\"size\"" | TypeOf -> "\"type\"" | Row -> "\"row\"" | Column -> "\"column\""

let rec string_of_expr = function
    LitInt(l) ->          "{\"LitInt\":" ^ string_of_int l ^ "}"
  | LitFlt(l) ->          "{\"LitFlt\":" ^ string_of_float l ^ "}"
  | LitString(s) ->       "{\"LitString\":" ^ quote_string s ^ "}"
  | LitRange(rowlist) ->  "{\"LitRange\": " ^ string_of_list (Rows rowlist) ^ "}"
  | Id(s) ->              "{\"Id\": " ^ quote_string s ^ "}"
  | Empty ->              "\"Empty\""
  | Wild ->               "\"Wild\""
  | BinOp(e1, o, e2) ->   "{\"BinOp\": {" ^
                            "\"expr1\": " ^ string_of_expr e1 ^ ", " ^
                            "\"operator\": " ^ string_of_op o ^ ", " ^
                            "\"expr2\": " ^ string_of_expr e2 ^ "}}"
  | UnOp(o, e) ->         "{\"UnOp\": {" ^
                            "\"operator\": " ^ string_of_unop o ^ ", " ^
                            "\"expr\": " ^ string_of_expr e ^ "}}"
  | Ternary(c, e1, e2) -> "{\"Ternary\": {" ^
                            "\"condition\": " ^ string_of_expr c ^ ", " ^
                            "\"ifExpr\": " ^ string_of_expr e1 ^ ", " ^
                            "\"elseExpr\": " ^ string_of_expr e2 ^ "}}"
  | Switch(eo, cases, dflt) ->  "{\"Switch\": {" ^
                                "\"condition\": " ^
                                  (match eo with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                                "\"cases\": " ^ string_of_list (Cases cases) ^ ", " ^
                                "\"defaultExpr\": " ^ string_of_expr dflt ^ "}}"
  | Call(f, arguments) -> "{\"Call\": {" ^
                            "\"function\": " ^ quote_string f ^ ", " ^
                            "\"arguments\": " ^ string_of_list (Exprs arguments) ^ "}}"
  | Selection(e, s) ->    "{\"Selection\": {" ^
                            "\"expr\": " ^ string_of_expr e ^ ", " ^
                            "\"slices\": " ^ string_of_sel s ^ "}}"
  | Precedence(e1, e2) -> "{\"Precedence\": { " ^
                            "\"prior_expr\": " ^ string_of_expr e1 ^ ", " ^
                            "\"dependent_expr\": " ^ string_of_expr e2 ^ "}}"

and string_of_case (el, e) =
    "{\"Cases\": " ^ string_of_list (Exprs el) ^ ", " ^
     "\"expr\": " ^ string_of_expr e ^ "}"

and string_of_sel (s1, s2) =
    "{\"slice1\": " ^ string_of_slice s1 ^ ", \"slice2\": " ^ string_of_slice s2 ^ "}"

and string_of_slice = function
    None -> "null"
  | Some (start_idx, end_idx) -> "{\"start\": " ^ string_of_index start_idx ^ ", \"end\": " ^ string_of_index end_idx ^ "}"

and string_of_index = function
    None -> "null"
  | Some(Abs(e)) -> "{\"Absolute\": " ^ string_of_expr e ^ "}"
  | Some(Rel(e)) -> "{\"Relative\": " ^ string_of_expr e ^ "}"
  | Some(DimensionStart) -> "\"DimensionStart\""
  | Some(DimensionEnd) -> "\"DimensionEnd\""

and string_of_dim (d1,d2) = "{\"d1\": " ^ (match d1 with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                             "\"d2\": " ^ (match d2 with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_var (d, s) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                            "\"VarName\": " ^ quote_string s ^ "}"

and string_of_assign (s, selection, eo) =
    "{\"VarName\": " ^ quote_string s ^ ", " ^
     "\"Selection\": " ^ string_of_sel selection ^ ", " ^
    "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_varinit (d, inits) =
  "{\"Dimensions\": " ^ string_of_dim d ^
    ",\"Initializations\": " ^ string_of_list (Inits inits) ^ "}"

and string_of_init (s, eo) =
    "{\"VarName\": " ^ quote_string s ^ ", " ^
     "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_stmt = function
    Assign(a) -> "{\"Assign\": " ^ string_of_assign a ^ "}"
  | Varinit(d, inits) -> "{\"Varinit\": " ^ string_of_varinit (d, inits) ^ "}"

and string_of_range (d, e) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                              "\"expr\": " ^ string_of_expr e ^ "}"

and string_of_raw_func fd =
    "{\"Name\": " ^ quote_string fd.name ^ "," ^
     "\"Params\": " ^ string_of_list (Vars fd.params) ^ "," ^
     "\"Stmts\": " ^ string_of_list (Stmts fd.body) ^ "," ^
     "\"Assertions\": " ^ string_of_list (Exprs fd.raw_asserts) ^ "," ^
     "\"ReturnVal\": " ^ string_of_range fd.ret_val ^ "}"

and string_of_extern_func fd =
  "{\"Name\": " ^ quote_string fd.extern_fn_name ^ "," ^
  "\"Params\": " ^ string_of_list (Vars fd.extern_fn_params) ^ "," ^
  "\"Library\": " ^ quote_string fd.extern_fn_libname ^ "," ^
  "\"ReturnDim\": " ^ string_of_dim fd.extern_ret_val ^ "}"

and string_of_library (Library(lib_name, lib_fns)) =
  "{\"LibraryName\": " ^ quote_string lib_name ^ "," ^
  "\"ExternalFunctions\": " ^ string_of_list (Externs lib_fns) ^ "}"

and string_of_dimexpr = function
    DimInt(i) -> string_of_int i
  | DimId(s) -> quote_string s

and string_of_formula f =
  "{\"RowStart\": " ^ string_of_index (Some f.formula_row_start) ^ "," ^
  "\"RowEnd\": " ^ string_of_index (f.formula_row_end) ^ "," ^
  "\"ColumnStart\": " ^ string_of_index (Some f.formula_col_start) ^ "," ^
  "\"ColumnEnd\": " ^ string_of_index (f.formula_col_end) ^ "," ^
  "\"Formula\": " ^ string_of_expr f.formula_expr ^ "}"

and string_of_list l =
  let stringrep = (match l with
    Inits (il) -> List.map string_of_init il
  | Vars(vl) -> List.map string_of_var vl
  | Stmts(sl) -> List.map string_of_stmt sl
  | RawFuncs(fl) -> List.map string_of_raw_func fl
  | Externs(efl) -> List.map string_of_extern_func efl
  | Libraries(libl) -> List.map string_of_library libl
  | Exprs(el) -> List.map string_of_expr el
  | Rows(rl) -> List.map (fun (el : expr list) -> string_of_list (Exprs el)) rl
  | Strings(sl) -> List.map quote_string sl
  | Cases(cl) -> List.map string_of_case cl
  | Formulas(fl) -> List.map string_of_formula fl)
  in "[" ^ String.concat ", " stringrep ^ "]"

let string_of_raw_program (imp, glb, fs, exts) =
    "{\"Program\": {" ^
      "\"Imports\": " ^ string_of_list (Strings imp) ^ "," ^
      "\"Globals\": " ^ string_of_list (Stmts glb) ^ "," ^
      "\"ExternalLibraries\": " ^ string_of_list (Libraries exts) ^ "," ^
      "\"Functions\": " ^ string_of_list (RawFuncs fs) ^ "}}"

let string_of_variable v =
  "{\"Rows\": " ^ string_of_dimexpr v.var_rows ^ "," ^
  "\"Columns\": " ^ string_of_dimexpr v.var_cols ^ "," ^
  "\"Formulas\": " ^ string_of_list (Formulas v.var_formulas) ^ "}"

let string_of_map value_desc val_printing_fn m =
  let f_key_val_list k v l = (
    "{\"" ^ value_desc ^ "Name\": " ^ quote_string k ^ ", " ^
    "\"" ^ value_desc ^ "Def\": " ^ val_printing_fn v ^ "}"
  ) :: l in
  "[" ^ String.concat ", " (List.rev (StringMap.fold f_key_val_list m [])) ^ "]"

let string_of_funcdecl f =
  "{\"Params\": " ^ string_of_list (Vars f.func_params) ^ "," ^
  "\"Variables\": " ^ string_of_map "Variable" string_of_variable f.func_body ^ "," ^
  "\"Assertions\": " ^ string_of_list (Exprs f.func_asserts) ^ "," ^
  "\"ReturnVal\": " ^ string_of_range f.func_ret_val ^ "}"

let string_of_program (glb, fs, exts) =
  "{\"Program\": {" ^
    "\"Globals\": " ^ string_of_map "Variable" string_of_variable glb ^ "," ^
    "\"Functions\": " ^ string_of_map "Function" string_of_funcdecl fs ^ "," ^
    "\"ExternalFunctions\": " ^ string_of_map "ExternalFunctions" string_of_extern_func exts ^ "}}"

let allow_range_literal = function
    LitRange(rowlist) ->
      let rec check_range_literal rl =
        List.for_all (fun exprs -> List.for_all check_basic_expr exprs) rl
      and check_basic_expr = function
          LitInt(_) | UnOp(Neg, LitInt(_)) | LitFlt(_) | UnOp(Neg, LitFlt(_)) | LitString(_) | Empty -> true
        | LitRange(rl) -> check_range_literal rl
        | _ -> false in

      if check_range_literal rowlist then LitRange(rowlist)
      else raise(IllegalRangeLiteral(string_of_expr (LitRange(rowlist))))
  | e -> raise(IllegalRangeLiteral(string_of_expr e))
