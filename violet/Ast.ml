type expr =
  | Econst of nativeint
  | Evar of string
  | Ecall of string * expr list
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr

and stmt =
  | Sempty
  | Sexpr of expr
  | Slabel of string * stmt
  | Sreturn of expr option
  | Sbreak
  | Scontinue
  | Sgoto of string
  | Sblock of stmt list
  | Sif of expr * stmt * stmt
  | Swhile of expr * stmt
  | Sloop of stmt

and binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Brem

and unop =
  | Iunop_neg

and function_decl = string * string list * stmt

and program = function_decl list
