type op       = Plus | Minus | Times | Divide | Mod | Pow |
                LShift | RShift | BitNot | BitOr | BitAnd | BitXor |
                Eq | NotEq | Gt | Lt | GtEq | LtEq | LogAnd | LogOr
type unop     = Neg | LogNot | BitNot

type expr     = LitInt of int |
                LitFlt of float |
                LitString of string |
                LitRange of (expr list) list |
                Id of string |
                Empty |
                Binop of expr * op * expr |
                Unop of unop * expr |
                Ternary of expr * expr * expr |
                Switch of expr option * case list |
                Call of string * expr list |
                Selection of expr * sel
and  index    = Abs of expr | Rel of expr
and  slice    = index option * index option
and  sel      = slice option * slice option
and  case     = (expr list) option * expr

type dim      = expr option * expr option
type var      = dim * string
type assign   = string * sel * expr
type stmt     = Assign of assign | Vardecl of var * assign option

type func_decl = {
    fname: string;
    params: var list;
    body: stmt list;
    ret_val: dim * expr;
}

type program = string list * stmt list * func_decl list
