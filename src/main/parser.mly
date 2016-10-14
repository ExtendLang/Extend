/* Ocamlyacc parser for Extend */

%{
open Ast
%}

%token LSQBRACK RSQBRACK LPAREN RPAREN LBRACE RBRACE
%token COLON COMMA QUESTION GETS EQ SEMI UNDERSCORE
%token SWITCH CASE DEFAULT
%token PLUS MINUS TIMES DIVIDE MOD
%token EMPTY RETURN
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <string> ID
%token EOF

%right QUESTION
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start program
%type <Ast.program> program

%%

program:
    func_decls EOF { List.rev $1 }

func_decls:
    /* nothing */ {[]}
  | func_decls func_decl {$2 :: $1}

func_decl:
    ID LPAREN func_param_list RPAREN LBRACE opt_stmt_list ret_stmt RBRACE
    { ((1,1), $1, $3, $6, $7) }
  | ret_dim ID LPAREN func_param_list RPAREN LBRACE opt_stmt_list ret_stmt RBRACE
    { ($1,    $2, $4, $7, $8) }

opt_stmt_list:
    /* nothing */ { [] }
  | stmt_list { List.rev $1 }

stmt_list:
    stmt { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    vardecl { $1 } |  assign { $1 }

ret_stmt:
    RETURN expr SEMI {$2}

vardecl:
    ID varassign SEMI {($1, $2)}
  | dim ID varassign SEMI {($1, $2, $3)}

varassign:
    /* nothing */ {}
  | GETS expr {($2)}

assign:
    ID lhs_sel EQ expr SEMI { ($1,$2,$4) }

expr:
    ID rhs_sel {($1,$2)}
  | op_expr { $1 }
  | ternary_expr { $1 }
  | switch_expr { $1 }
  | func_expr { $1 }
  | LPAREN expr RPAREN { $2 }
  | LIT_INT { $1 }
  | LIT_FLOAT { $1 }
  | EMPTY { Empty }

op_expr:
    expr PLUS expr { ($1, $3) }
  | expr MINUS expr { ($1, $3) }
  | expr TIMES expr { ($1, $3) }
  | expr DIVIDE expr { ($1, $3) }
  | expr MOD expr { ($1, $3) }

ternary_expr:
  /* commented out optional part for now */
    expr QUESTION expr COLON expr %prec QUESTION { ($1, $3, $5) }

switch_expr:
    SWITCH switch_cond LBRACE case_list RBRACE { ($2, List.rev $4) }

switch_cond:
    /* nothing */ { True }
  | expr { $1 }

case_list:
    case_stmt { [$1] }
  | case_list case_stmt { $2 :: $1 }

case_stmt:
    DEFAULT COLON expr SEMI { $3 }
  | CASE case_expr_list COLON expr SEMI { (List.rev $2, $4) }

case_expr_list:
    expr { [$1] }
  | case_expr_list COMMA expr { $3 :: $1 }

func_expr:
    ID LPAREN opt_arg_list RPAREN { $3 }

opt_arg_list:
    /* nothing */ {[]}
  | arg_list { List.rev $1 }

arg_list:
    expr {[$1]}
  | arg_list COMMA expr {$3 :: $1}

lhs_sel:
    /* nothing */ { [0,0] }
  | LSQBRACK lslice COMMA lslice RSQBRACK { ($2,$4) }
  | LSQBRACK lslice RSQBRACK { ($2) }

rhs_sel:
    /* nothing */ { [0,0] }
  | LSQBRACK rslice COMMA rslice RSQBRACK { ($2,$4) }
  | LSQBRACK rslice RSQBRACK { ($2) }

lslice:
    /* nothing */ { 0 }
  | lslice_val { $1 }
  | lslice_val COLON lslice_val { ($1,$3) }

rslice:
    /* nothing */ { 0 }
  | rslice_val { $1 }
  | rslice_val COLON rslice_val { ($1,$3) }

lslice_val:
    expr { $1 }

rslice_val:
    expr { $1 }
  | LSQBRACK expr RSQBRACK { ($2) }

func_param_list:
    /* nothing */ { [] }
  | func_param_int_list { List.rev $1 }

func_param_int_list:
    func_sin_param { [$1] }
  | func_param_int_list COMMA func_sin_param { $3 :: $1 }

func_sin_param:
    ID { ($1) }
  | dim ID { ($1, $2) }

dim:
    LSQBRACK lslice_val RSQBRACK { $2 }
  | LSQBRACK lslice_val COMMA lslice_val RSQBRACK { ($2,$4) }

ret_dim:
  LSQBRACK ret_sin COMMA ret_sin RSQBRACK { ($2,$4) }

ret_sin:
    LIT_INT { $1 }
  | ID { $1 }
  | UNDERSCORE {}
