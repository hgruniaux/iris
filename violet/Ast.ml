type expr =
  | Eint of Z.t
  | Estring of string
  | Evar of string
  | Enew of Z.t
  | Ecall of string * expr list
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr

and stmt =
  | Sempty
  | Sexpr of expr
  | Svardecl of string * expr
  | Sprint of expr
  | Slabel of string * stmt
  | Sreturn of expr option
  | Sbreak
  | Scontinue
  | Sgoto of string
  | Sdelete of expr
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
  | Band
  | Bland
  | Bor
  | Blor
  | Bxor
  | Bshift_left
  | Bshift_right
  | Beq
  | Bne
  | Blt
  | Ble
  | Bgt
  | Bge

and unop =
  | Uneg
  | Unot

and function_decl = string * string list * stmt

and program = function_decl list
