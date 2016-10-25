type op       = Plus | Minus | Times | Divide | Mod | Pow |
                LShift | RShift | BitOr | BitAnd | BitXor |
                Eq | NotEq | Gt | Lt | GtEq | LtEq | LogAnd | LogOr
type unop     = Neg | LogNot | BitNot

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
                Switch of expr option * case list |
                Call of string * expr list |
                Selection of expr * sel |
                Precedence of expr * expr
and  index    = Abs of expr | Rel of expr | DimensionStart | DimensionEnd
and  slice    = index option * index option
and  sel      = slice option * slice option
and  case     = (expr list) option * expr

type dim      = expr option * expr option
type var      = dim * string
type assign   = string * sel * expr option
type init     = string * expr option
type stmt     = Assign of assign | Vardecl of dim * init list

type func_decl = {
    name: string;
    params: var list;
    body: stmt list;
    ret_val: dim * expr;
}

type listable = Inits of init list|
                Vars of var list |
                Stmts of stmt list |
                Funcs of func_decl list |
                Exprs of expr list |
                Rows of (expr list) list |
                Strings of string list |
                Cases of case list

type program = string list * stmt list * func_decl list

let escape_characters = Str.regexp "[\n \t \r \\ \"]"
let replace_fn s = match Str.matched_string s with
  "\n" -> "\\n"   |
  "\t" -> "\\t"   |
  "\r" -> "\\r"   |
  "\\" -> "\\\\"  |
  "\"" -> "\\\""  |
  _    -> Str.matched_string s in
let quote_string str = "\"" ^ Str.global_substitute escape_characters replace_fn s ^ "\""

let string_of_op o = "\"" ^ (match o with
    Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/" | Mod -> "%" | Pow -> "**" |
    LShift -> "<<" | RShift -> ">>" | BitOr -> "|" | BitAnd -> "&" | BitXor -> "^" |
    Eq -> "==" | NotEq -> "!-" | Gt -> ">" | Lt -> "<" | GtEq -> ">=" | LtEq -> "<=" |
    LogAnd -> "&& " | LogOr -> "||" ) ^ "\""

let string_of_unop = function
    Neg -> "\"-\"" | LogNot -> "\"!\"" | BitNot -> "\"~\""

let rec comma = function
    [] -> ""
  | [str] -> str
  | str :: strs -> str ^ ", " ^ comma strs

let rec string_of_expr = function
    LitInt(l) ->          "{\"LitInt\":" ^ string_of_int l ^ "}"
  | LitFlt(l) ->          "{\"LitFlt\":" ^ string_of_float l ^ "}"
  | LitString(s) ->       "{\"LitString\":" ^ quote_string s ^ "}"
  | LitRange(rowlist) ->  "{\"LitRange\": " ^ string_of_list (Rows rowlist) ^ "}"
  | Id(s) ->              "{\"Id\": " ^ quote_string s ^ "}"
  | Empty ->              "{\"Empty\": null}"
  | Wild ->               "{\"Wild\": null}"
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
  | Switch(eo, cases) ->  "{\"Switch\": {" ^
                            "\"condition\": " ^
                              (match eo with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                            "\"cases\": " ^ string_of_list (Cases cases) ^ "}}"
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
    "{\"Cases\": " ^ (match el with None -> "null" | Some es -> string_of_list (Exprs es)) ^ ", " ^
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
  | Some(DimensionStart) -> "{\"DimensionStart\": null}"
  | Some(DimensionEnd) -> "{\"DimensionEnd\": null}"

and string_of_dim (d1,d2) = "{\"d1\": " ^ (match d1 with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                             "\"d2\": " ^ (match d2 with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_var (d, s) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                            "\"VarName\": " ^ quote_string s ^ "}"

and string_of_assign (s, selection, eo) =
    "{\"VarName\": " ^ quote_string s ^ ", " ^
     "\"Selection\": " ^ string_of_sel selection ^ ", " ^
     "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_init (s, eo) =
    "{\"VarName\": " ^ quote_string s ^ ", " ^
     "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

and string_of_stmt = function
    Assign(a) -> "{\"Assign\": " ^ string_of_assign a ^ "}"
  | Vardecl(d, inits) -> "{\"Vardecl\": {\"Dimensions\": " ^ string_of_dim d ^
                                       ",\"Initializations\": " ^ string_of_list (Inits inits) ^ "}}"

and string_of_range (d, e) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                              "\"expr\": " ^ string_of_expr e ^ "}"

and string_of_funcdecl fd =
    "{\"Name\": " ^ quote_string fd.name ^ "," ^
     "\"Params\": " ^ string_of_list (Vars fd.params) ^ "," ^
     "\"Stmts\": " ^ string_of_list (Stmts fd.body) ^ "," ^
     "\"ReturnVal\": " ^ string_of_range fd.ret_val ^ "}"

and string_of_list l =
  let stringrep = (match l with
    Inits (il) -> List.map string_of_init il
  | Vars(vl) -> List.map string_of_var vl
  | Stmts(sl) -> List.map string_of_stmt sl
  | Funcs(fl) -> List.map string_of_funcdecl fl
  | Exprs(el) -> List.map string_of_expr el
  | Rows(rl) -> List.map (fun (el : expr list) -> string_of_list (Exprs el)) rl
  | Strings(sl) -> List.map quote_string sl
  | Cases(cl) -> List.map string_of_case cl)
  in "[" ^ comma stringrep ^ "]"

let string_of_program (imp, glb, fs) =
    "{\"Program\": {" ^
      "\"Imports\": " ^ string_of_list (Strings imp) ^ "," ^
      "\"Globals\": " ^ string_of_list (Stmts glb) ^ "," ^
      "\"Functions\": " ^ string_of_list (Funcs fs) ^ "}}"
