/* Ocamlyacc parser for Extend */

%{
open Ast
%}

%token LSQBRACK RSQBRACK LPAREN RPAREN LBRACE RBRACE HASH
%token COLON COMMA QUESTION GETS ASN SEMI UNDERSCORE
%token SWITCH CASE DEFAULT
%token PLUS MINUS TIMES DIVIDE MOD POWER LSHIFT RSHIFT
%token EQ NOTEQ GT LT GTEQ LTEQ
%token LOGNOT LOGAND LOGOR
%token BITNOT BITXOR BITAND BITOR
%token EMPTY RETURN IMPORT GLOBAL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <string> LIT_STRING
%token <string> ID
%token EOF

%right QUESTION
%left LOGOR
%left LOGAND
%left EQ NOTEQ LT GT LTEQ GTEQ
%left PLUS MINUS BITOR BITXOR
%left TIMES DIVIDE MOD LSHIFT RSHIFT BITAND
%left POWER
%right BITNOT LOGNOT NEG
%left HASH LSQBRACK

%start program
%type <Ast.program> program

%%

program:
    imports globals func_decls EOF { (List.rev $1, List.rev $2, List.rev $3) }

imports:
    /* nothing */ {[]}
  | imports import {$2 :: $1}

import:
    IMPORT LIT_STRING SEMI {$2}

globals:
    /* nothing */ {[]}
  | globals global {$2 :: $1}

global:
    GLOBAL vardecl {$2}

func_decls:
    /* nothing */ {[]}
  | func_decls func_decl {$2 :: $1}

func_decl:
    ID LPAREN func_param_list RPAREN LBRACE opt_stmt_list ret_stmt RBRACE
    { {
      ret_val: ((ABS, 1), (ABS, 1));
      name: $1;
      params: $3;
      body: $6;
      ret_stmt: $7
    } }
  | ret_dim ID LPAREN func_param_list RPAREN LBRACE opt_stmt_list ret_stmt RBRACE
    { {
      ret_val: $1;
      name: $2;
      params: $4;
      body: $7;
      ret_stmt: $8;
    } }

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
    ID lhs_sel ASN expr SEMI { ($1,$2,$4) }

expr:
    expr rhs_sel { ($1, $2) }
  | HASH expr { ($2) }
  | op_expr { $1 }
  | ternary_expr { $1 }
  | switch_expr { $1 }
  | func_expr { $1 }
  | range_expr { $1 }
  | LPAREN expr RPAREN { $2 }
  | ID { $1 }
  | LIT_INT { $1 }
  | LIT_FLOAT { $1 }
  | LIT_STRING { $1 }
  | EMPTY { Empty }

op_expr:
    expr PLUS expr { ($1, $3) }
  | expr MINUS expr { ($1, $3) }
  | expr TIMES expr { ($1, $3) }
  | expr DIVIDE expr { ($1, $3) }
  | expr MOD expr { ($1, $3) }
  | expr POWER expr { ($1, $3) }
  | expr LSHIFT expr { ($1, $3) }
  | expr RSHIFT expr { ($1, $3) }
  | expr LOGAND expr { ($1, $3) }
  | expr LOGOR expr { ($1, $3) }
  | expr BITXOR expr { ($1, $3) }
  | expr BITAND expr { ($1, $3) }
  | expr BITOR expr { ($1, $3) }
  | expr EQ expr { ($1, $3) }
  | expr NOTEQ expr { ($1, $3) }
  | expr GT expr { ($1, $3) }
  | expr LT expr { ($1, $3) }
  | expr GTEQ expr { ($1, $3) }
  | expr LTEQ expr { ($1, $3) }
  | MINUS expr %prec NEG { ($2) }
  | LOGNOT expr { ($2) }
  | BITNOT expr { ($2) }

ternary_expr:
  /* commented out optional part for now */
    expr QUESTION expr COLON expr %prec QUESTION { ($1, $3, $5) }

switch_expr:
    SWITCH LPAREN switch_cond RPAREN LBRACE case_list RBRACE { ($3, List.rev $6) }

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

range_expr:
    LBRACE row_list RBRACE { List.rev $2 }

row_list:
    col_list {[List.rev $1]}
  | row_list SEMI col_list {$3 :: $1}

col_list:
    expr {$1}
  | col_list COMMA expr {$3 :: $1}

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
    LSQBRACK rslice COMMA rslice RSQBRACK { ($2,$4) }
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
    LSQBRACK expr RSQBRACK { (SINGLE,$2,0) }
  | LSQBRACK expr COMMA expr RSQBRACK { (DOUBLE,$2,$4) }

ret_dim:
  LSQBRACK ret_sin COMMA ret_sin RSQBRACK { ($2,$4) }

ret_sin:
    expr { $1 }
  | UNDERSCORE { (WILD, 0) }
