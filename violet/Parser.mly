%{
  open Ast
%}

%token EOF
%token <string> IDENT
%token <nativeint> INTEGER
%token <string> STRING
%token COLON COMMA SEMI LPAR RPAR LBRACKET RBRACKET
%token PLUS MINUS STAR SLASH PERCENT LESS_LESS GREATER_GREATER
%token AMP AMP_AMP PIPE PIPE_PIPE CARET EQ PRINT
%token FN LET RETURN IF ELSE WHILE LOOP BREAK CONTINUE GOTO TRUE FALSE NEW DELETE
%token EQ_EQ EXCLAIM_EQ LESS LESS_EQ GREATER GREATER_EQ

%nonassoc THEN ELSE
%left PIPE_PIPE
%left AMP_AMP
%left PIPE
%left CARET
%left AMP
%left EQ_EQ EXCLAIM_EQ LESS LESS_EQ GREATER GREATER_EQ
%left LESS_LESS GREATER_GREATER
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
| FALSE { Eint 0n }
| TRUE { Eint 1n }
| i=INTEGER { Eint i }
| s=STRING { Estring s }
| i=IDENT { Evar i }
| LPAR e=expr RPAR { e }
| NEW s=INTEGER { Enew s }
| l=expr o=binop r=expr { Ebinop (o, l, r) }
| MINUS r=expr %prec NEG { Eunop (Iunop_neg, r) }
| i=IDENT LPAR args=separated_list(COMMA, expr) RPAR { Ecall (i, args) }

stmt:
| SEMI { Sempty }
| e=expr SEMI { Sexpr e }
| i=IDENT COLON s=stmt { Slabel (i, s) }
| LET n=IDENT EQ e=expr SEMI { Svardecl (n, e) }
| PRINT e=expr SEMI { Sprint (e) }
| RETURN e=expr? SEMI { Sreturn e }
| BREAK SEMI { Sbreak }
| CONTINUE SEMI { Scontinue }
| DELETE e=expr SEMI { Sdelete e }
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
| LESS_LESS { Bshift_left }
| GREATER_GREATER { Bshift_right }
| AMP { Band }
| AMP_AMP { Bland }
| PIPE { Bor }
| PIPE_PIPE { Blor }
| CARET { Bxor }
| EQ_EQ { Beq }
| EXCLAIM_EQ { Bne }
| LESS { Blt }
| LESS_EQ { Ble }
| GREATER { Bgt }
| GREATER_EQ { Bge }
