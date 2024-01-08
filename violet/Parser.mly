%{
  open Ast
%}

%token EOF
%token <string> IDENT
%token <nativeint> INTEGER
%token COLON COMMA SEMI LPAR RPAR LBRACKET RBRACKET
%token PLUS MINUS STAR SLASH PERCENT LESS_LESS GREATER_GREATER
%token FN LET RETURN IF ELSE WHILE LOOP BREAK CONTINUE GOTO

%nonassoc THEN ELSE
%left PIPE_PIPE
%left AMP_AMP
%nonassoc EQ_EQ SLASH_EQ LESS LESS_EQ GREATER GREATER_EQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NEG

%start <Ast.program> program
%%

program:
| decls=function_decl* EOF
  { decls }

function_decl:
| FN n=IDENT LPAR params=separated_list(COMMA, param) RPAR b=block_stmt
  { (n, params, b) }

param:
| i=IDENT { i }

expr:
| i=INTEGER { Econst i }
| i=IDENT { Evar i }
| LPAR e=expr RPAR { e }
| l=expr o=binop r=expr { Ebinop (o, l, r) }
| MINUS r=expr %prec NEG { Eunop (Iunop_neg, r) }
| i=IDENT LPAR args=separated_list(COMMA, expr) RPAR { Ecall (i, args) }

stmt:
| SEMI { Sempty }
| e=expr SEMI { Sexpr e }
| i=IDENT COLON s=stmt { Slabel (i, s) }
| RETURN e=expr? SEMI { Sreturn e }
| BREAK SEMI { Sbreak }
| CONTINUE SEMI { Scontinue }
| GOTO i=IDENT SEMI { Sgoto i }
| s=if_stmt { s }
| WHILE c=expr b=block_stmt { Swhile (c, b) }
| LOOP b=block_stmt { Sloop b }

if_stmt:
| IF c=expr t=block_stmt %prec THEN { Sif (c, t, Sempty) }
| IF c=expr t=block_stmt ELSE e=block_stmt { Sif (c, t, e) }
| IF c=expr t=block_stmt ELSE e=if_stmt { Sif (c, t, e) }

block_stmt:
| LBRACKET stmts=stmt* RBRACKET { Sblock (stmts) }

%inline binop:
| PLUS { Badd }
| MINUS { Bsub }
| STAR { Bmul }
| SLASH { Bdiv }
| PERCENT { Brem }
