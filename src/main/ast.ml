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

type program = string list * stmt list * func_decl list

let string_of_op = function
    Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/" | Mod -> "%" | Pow -> "**" |
    LShift -> "<<" | RShift -> ">>" | BitOr -> "|" | BitAnd -> "&" | BitXor -> "^" |
    Eq -> "==" | NotEq -> "!-" | Gt -> ">" | Lt -> "<" | GtEq -> ">=" | LtEq -> "<=" | LogAnd -> "&& " | LogOr -> "||"

let string_of_unop = function
    Neg -> "-" | LogNot -> "!" | BitNot -> "~"

let rec string_of_expr = function
    LitInt(l) ->          "{\"LitInt\":" ^ string_of_int l ^ "}"
  | LitFlt(l) ->          "{\"LitFlt\":" ^ string_of_float l ^ "}"
  | LitString(s) ->       "{\"LitString\":\"" ^ s (* TODO: Escape the string *) ^ "\"}"
  | LitRange(rowlist) ->  "{\"LitRange\": [" ^ string_of_rowlist rowlist ^ "]}"
  | Id(s) ->              "{\"Id\": \"" ^ s (* TODO: Escape the string *) ^ "\"}"
  | Empty ->              "{\"Empty\": null}"
  | Wild ->               "{\"Wild\": null}"
  | BinOp(e1, o, e2) ->   "{\"BinOp\": {" ^
                            "\"expr1\": " ^ string_of_expr e1 ^ ", " ^
                            "\"operator\": \"" ^ string_of_op o ^ "\", " ^
                            "\"expr2\": " ^ string_of_expr e2 ^ "}}"
  | UnOp(o, e) ->         "{\"UnOp\": {" ^
                            "\"operator\": \"" ^ string_of_unop o ^ "\", " ^
                            "\"expr\": " ^ string_of_expr e ^ "}}"
  | Ternary(c, e1, e2) -> "{\"Ternary\": {" ^
                            "\"condition\": " ^ string_of_expr c ^ ", " ^
                            "\"ifExpr\": " ^ string_of_expr e1 ^ ", " ^
                            "\"elseExpr\": " ^ string_of_expr e2 ^ "}}"
  | Switch(eo, cases) ->  "{\"Switch\": {" ^
                            "\"condition\": " ^
                              (match eo with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                            "\"cases\": [" ^ string_of_cases cases ^ "]}}"
  | Call(f, arguments) -> "{\"Call\": {" ^
                            "\"function\": \"" ^ f (* TODO: Escape the string *) ^ "\", " ^
                            "\"arguments\": [" ^ string_of_exprs arguments ^ "]}}"
  | Selection(e, s) ->    "{\"Selection\": {" ^
                            "\"expr\": " ^ string_of_expr e ^ ", " ^
                            "\"slices\": " ^ string_of_sel s ^ "}}"
  | Precedence(e1, e2) -> "{\"Precedence\": { " ^
                            "\"prior_expr\": " ^ string_of_expr e1 ^ ", " ^
                            "\"dependent_expr\": " ^ string_of_expr e2 ^ "}}"

(* TODO: Do this in more idiomatic OCaml *)
and string_of_rowlist = function
    [] -> "[{\"error\":\"This shouldnt be possible\"}]"
  | [row] -> "[" ^ string_of_exprs row ^ "]"
  | row :: rows -> "[" ^ string_of_exprs row ^ "], " ^ string_of_rowlist rows

and string_of_exprs = function
    [] -> "[{\"error\":\"This shouldn\'t be possible\"}]"
  | [col] -> string_of_expr col
  | col :: cols -> string_of_expr col ^ ", " ^ string_of_exprs cols

and string_of_case (el, e) =
    "{\"Cases\": " ^ (match el with None -> "null" | Some es -> "[" ^ string_of_exprs es ^ "]") ^ ", " ^
     "\"expr\": " ^ string_of_expr e ^ "}"

and string_of_cases = function
    [c] -> string_of_case c
  | c :: cs -> string_of_case c ^ ", " ^ string_of_cases cs
  | [] -> "[{\"error\":\"This shouldn\'t be possible\"}]"

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

let string_of_dim (d1,d2) = "{\"d1\": " ^ (match d1 with None -> "null" | Some e -> string_of_expr e) ^ ", " ^
                             "\"d2\": " ^ (match d2 with None -> "null" | Some e -> string_of_expr e) ^ "}"

let string_of_var (d, s) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                            "\"VarName\": \"" ^ s (* TODO: escape the string *) ^ "\"}"

let string_of_assign (s, selection, eo) =
    "{\"VarName\": \"" ^ s (* TODO: escape the string *) ^ "\", " ^
     "\"Selection\": " ^ string_of_sel selection ^ ", " ^
     "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

let string_of_init (s, eo) =
    "{\"VarName\": \"" ^ s (* TODO: escape the string *) ^ "\", " ^
     "\"expr\": " ^ (match eo with None -> "null" | Some e -> string_of_expr e) ^ "}"

let rec string_of_inits = function
    [] -> "[{\"error\":\"This shouldn\'t be possible\"}]"
  | [init] -> string_of_init init
  | init :: inits -> string_of_init init ^ ", " ^ string_of_inits inits

let string_of_stmt = function
    Assign(a) -> "{\"Assign\": " ^ string_of_assign a ^ "}"
  | Vardecl(d, inits) -> "{\"Vardecl\": {\"Dimensions\": " ^ string_of_dim d ^
                                       ",\"Initializations\": [" ^ string_of_inits inits ^ "]}}"

let rec string_of_vars = function
    [] -> ""
  | [v] -> string_of_var v
  | v :: vs -> string_of_var v ^ ", " ^ string_of_vars vs

let rec string_of_stmts = function
    [] -> ""
  | [st] -> string_of_stmt st
  | st :: sts -> string_of_stmt st ^ ", " ^ string_of_stmts sts

let string_of_range (d, e) = "{\"Dimensions\": " ^ string_of_dim d ^ ", " ^
                              "\"expr\": " ^ string_of_expr e ^ "}"

let string_of_funcdecl fd =
    "{\"Name\": \"" ^ fd.name (* TODO: Escape the string *) ^ "\"," ^
     "\"Params\": [" ^ string_of_vars fd.params ^ "]," ^
     "\"Stmts\": [" ^ string_of_stmts fd.body ^ "]," ^
     "\"ReturnVal\": " ^ string_of_range fd.ret_val ^ "}"

let rec string_of_strs = function
    [] -> ""
  | [str] -> "\"" ^ str (* TODO: Escape the string *) ^ "\""
  | str :: strs -> "\"" ^ str (* TODO: Escape the string *) ^ "\", " ^ string_of_strs strs

let rec string_of_funcs = function
    [] -> ""
  | [f] -> string_of_funcdecl f
  | f :: fs -> string_of_funcdecl f ^ ", " ^ string_of_funcs fs

let string_of_program (imp, glb, fs) =
    "{\"Program\": {" ^
      "\"Imports\": [" ^ string_of_strs imp ^ "]," ^
      "\"Globals\": [" ^ string_of_stmts glb ^ "]," ^
      "\"Functions\": [" ^ string_of_funcs fs ^ "]}}"
