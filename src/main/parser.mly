/* Ocamlyacc parser for Extent */

%{
open Ast
%}

%token LSQBRACK RSQBRACK COLON
%token COMMA
%token QUESTION COLON
%token GETS EQ
%token SEMI
%token LPAREN RPAREN LBRACE RBRACE
%token ADD
%token EMPTY RETURN
%token INT STRING
%token <int> LITERAL
%token <string> ID
%token EOF

%left ADD
%right QUESTION

%start program
%type <Ast.program> program

%%

program:
  func_decls EOF { $1 }

func_decls:
    /* nothing */ {[]}
  | func_decls func_decl {$2 :: $1}

func_decl:
  ret_dim ID LPAREN func_param_list RPAREN LBRACE stmt_list ret_stmt RBRACE
   { [$1, $2, $4, $7, $8] }

stmt_list:
    /* nothing */ { [] }
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
  | opexpr { ($1) }
  | ternary_exp { ($1) }
  | EMPTY { Empty }

opexpr:
  expr ADD expr { ($1, $3) }

ternary_exp:
  /* commented out optional part for now */
  expr QUESTION expr COLON expr %prec QUESTION { ($1, $3,$5) }

rhs_sel:
    /* nothing */ { [0,0] }
  | LSQBRACK rslice COMMA rslice RSQBRACK { [$2,$4] }
  | LSQBRACK rslice RSQBRACK { [$2] }

rslice:
    LSQBRACK slice_val RSQBRACK { $2 }
  | LSQBRACK slice_val RSQBRACK COLON LSQBRACK slice_val RSQBRACK { ($2,$6) }
  | slice_val COLON slice_val { ($1,$3) }
  | slice_val { $1 }

lhs_sel:
    /* nothing */ { [0,0] }
  | LSQBRACK lslice COMMA lslice RSQBRACK { [$2,$4] }
  | LSQBRACK lslice RSQBRACK { [$2] }

lslice:
    /* nothing */ { 0 }
  | slice_val COLON slice_val { ($1,$3) }
  | slice_val { $1 }

slice_val:
    LITERAL { $1 }

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
    LSQBRACK slice_val RSQBRACK { $2 }
  | LSQBRACK slice_val COMMA slice_val RSQBRACK { ($2,$4) }

op:
  ADD { Add }

ret_dim:
  LSQBRACK ret_sin COMMA ret_sin RSQBRACK { ($2,$4) }

ret_sin:
    INT { Int }
  | ID { $1 }
