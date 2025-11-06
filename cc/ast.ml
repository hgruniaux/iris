type location = Lexing.position * Lexing.position
type identifier = { id : string; loc : location }
type 'a with_location = { value : 'a; loc : location }

(** The supported binary operators. *)
type binop_kind =
  | Pbinop_add
  | Pbinop_sub
  | Pbinop_mul
  | Pbinop_div
  | Pbinop_mod
  | Pbinop_and
  | Pbinop_or
  | Pbinop_xor
  | Pbinop_shl
  | Pbinop_shr
  | Pbinop_eq
  | Pbinop_ne
  | Pbinop_lt
  | Pbinop_le
  | Pbinop_gt
  | Pbinop_ge
  | Pbinop_logand
  | Pbinop_logor
  | Pbinop_assign
  | Pbinop_assign_add
  | Pbinop_assign_sub
  | Pbinop_assign_mul
  | Pbinop_assign_div
  | Pbinop_assign_mod
  | Pbinop_assign_and
  | Pbinop_assign_or
  | Pbinop_assign_xor
  | Pbinop_assign_shl
  | Pbinop_assign_shr
  | Pbinop_comma

and binop = { pbinop_kind : binop_kind; pbinop_loc : location }

(** The supported unary operators. *)
type unop_kind =
  | Punop_plus
  | Punop_neg
  | Punop_bitnot
  | Punop_lognot
  | Punop_deref
  | Punop_addrof
  | Punop_sizeof  (** sizeof on a expr, for type see pexpr_kind *)
  | Punop_pre_inc
  | Punop_pre_dec
  | Punop_post_inc
  | Punop_post_dec

and unop = { punop_kind : unop_kind; punop_loc : location }

(** The different suffixes that can be attached to an integer literal. *)
type int_suffix =
  | Pint_suffix_none  (** no suffixes *)
  | Pint_suffix_unsigned  (** u or U suffixes *)
  | Pint_suffix_long  (** l or L suffixes *)
  | Pint_suffix_unsigned_long  (** ul or UL suffixes *)
  | Pint_suffix_long_long  (** ll or LL suffixes *)
  | Pint_suffix_unsigned_long_long  (** e.g. ull or ULL suffixes *)

(** The different suffixes that can be attached to a floating-point literal. *)
type float_suffix =
  | Pfloat_suffix_none  (** no suffixes *)
  | Pfloat_suffix_float  (** f or F suffixes *)
  | Pfloat_suffix_long_double  (** l or L suffixes *)

and pstruct_specifier = identifier option * pstruct_member list option

and pstruct_member_kind =
  | Pstruct_member_field of ptype * pdeclarator list
  | Pstruct_member_static_assert of pexpr * pexpr option

and pstruct_member = pstruct_member_kind with_location

and penum_specifier =
  identifier option * ptype option * (identifier * pexpr option) list option

and ptype_specifier =
  | Ptype_specifier_void
  | Ptype_specifier_bool
  | Ptype_specifier_char
  | Ptype_specifier_short
  | Ptype_specifier_int
  | Ptype_specifier_long
  | Ptype_specifier_float
  | Ptype_specifier_double
  | Ptype_specifier_signed
  | Ptype_specifier_unsigned
  | Ptype_specifier_typedef_name of identifier
  | Ptype_specifier_struct of pstruct_specifier
  | Ptype_specifier_union of pstruct_specifier
  | Ptype_specifier_enum of penum_specifier
  | Ptype_specifier_typeof_expr of pexpr * bool (* is_typeof_unqual *)
  | Ptype_specifier_typeof_type of ptype_name * bool (* is_typeof_unqual *)

and ptype_qualifier =
  | Ptype_qualifier_const
  | Ptype_qualifier_volatile
  | Ptype_qualifier_restrict
  | Ptype_qualifier_alignas_expr of pexpr
  | Ptype_qualifier_alignas_type of ptype_name

and pfunction_specifier =
  | Pfunction_specifier_inline
  | Pfunction_specifier_noreturn

and pstorage_class_specifier =
  | Pstorage_class_specifier_extern
  | Pstorage_class_specifier_static
  | Pstorage_class_specifier_thread_local
  | Pstorage_class_specifier_auto
  | Pstorage_class_specifier_register
  | Pstorage_class_specifier_typedef

and pdeclaration_specifier_kind =
  | Ptype_specifier of ptype_specifier
  | Ptype_qualifier of ptype_qualifier
  | Pfunction_specifier of pfunction_specifier
  | Pstorage_class_specifier of pstorage_class_specifier

and pdeclaration_specifier = pdeclaration_specifier_kind with_location

and ptype = pdeclaration_specifier list
and ptype_name = ptype * pabstract_declarator option

and pdeclarator_kind =
  | Pdeclarator_name of identifier
  | Pdeclarator_paren of pdeclarator
  | Pdeclarator_pointer of pdeclarator * ptype_qualifier list
  | Pdeclarator_array of pdeclarator * ptype_qualifier list * pexpr option
  | Pdeclarator_array_static of pdeclarator * ptype_qualifier list * pexpr
  | Pdeclarator_array_star of pdeclarator * ptype_qualifier list
  | Pdeclarator_function of pdeclarator * pparameter list * bool * Context.t

and pdeclarator = pdeclarator_kind with_location

and pabstract_declarator =
  | Pabstract_declarator_paren of pabstract_declarator
  | Pabstract_declarator_pointer of
      pabstract_declarator option * ptype_qualifier list
  | Pabstract_declarator_array of
      pabstract_declarator * ptype_qualifier list * pexpr option
  | Pabstract_declarator_array_static of
      pabstract_declarator * ptype_qualifier list * pexpr
  | Pabstract_declarator_array_star of
      pabstract_declarator * ptype_qualifier list
  | Pabstract_declarator_function of
      pabstract_declarator option * pparameter list * bool

and pparameter_kind =
  | Pparameter_named of ptype * pdeclarator
  | Pparameter_unnamed of ptype_name

and pparameter = pparameter_kind with_location

and pexpr_kind =
  (* C23 6.5.2 Primary expressions *)
  | Pexpr_nullptr
  | Pexpr_bool of bool
  | Pexpr_int of Z.t * int_suffix
  | Pexpr_float of float * float_suffix
  | Pexpr_string of string list
  | Pexpr_char of string
    (* we store a list of strings to support string literal concatenation *)
  | Pexpr_ident of identifier
  | Pexpr_paren of pexpr
  (* C23 6.5.3 Postfix expressions *)
  | Pexpr_array of pexpr * pexpr
  | Pexpr_call of pexpr * pexpr list
  | Pexpr_member of pexpr * identifier
  | Pexpr_member_deref of pexpr * identifier
  (* C23 6.5.4 Unary operators *)
  | Pexpr_unary of unop * pexpr
  | Pexpr_sizeof of ptype_name
  | Pexpr_alignof of ptype_name
  (* C23 6.5.5 Cast operators *)
  | Pexpr_cast of ptype_name * pexpr
  (* C23 6.5.16 Conditional operator *)
  | Pexpr_conditional of pexpr * pexpr * pexpr
  (* C23 6.5.6 - 6.5.15 and 6.5.17 - 6.5.18 *)
  | Pexpr_binary of binop * pexpr * pexpr

and pexpr = { pexpr_kind : pexpr_kind; pexpr_loc : location }

and pstmt_kind =
  | Pstmt_decl of pdecl
  (* C23 6.8.2 Labeled statements *)
  | Pstmt_label of identifier * pstmt
  | Pstmt_case of pexpr * pstmt
  | Pstmt_default of pstmt
  (* C23 6.8.3 Compound statement *)
  | Pstmt_compound of pstmt list
  (* C23 6.8.4 Expression and null statements *)
  | Pstmt_null
  | Pstmt_expr of pexpr
  (* C23 6.8.5 Selection statements *)
  | Pstmt_if of pexpr * pstmt * pstmt option
  | Pstmt_switch of pexpr * pstmt
  (* C23 6.8.6 Iteration statements *)
  | Pstmt_while of pexpr * pstmt
  | Pstmt_do of pstmt * pexpr
  | Pstmt_for of pstmt option * pexpr option * pexpr option * pstmt
  (* 6.8.7 Jump statements *)
  | Pstmt_goto of identifier
  | Pstmt_continue
  | Pstmt_break
  | Pstmt_return of pexpr option

and pstmt = { pstmt_kind : pstmt_kind; pstmt_loc : location }

and pdecl_kind =
  | Pdecl_null (* for empty declarations *)
  | Pdecl_variable of ptype * (pdeclarator * pexpr option) list
  | Pdecl_typedef of ptype * (pdeclarator * pexpr option) list
  | Pdecl_static_assert of pexpr * pexpr option
  | Pdecl_function_definition of ptype * pdeclarator * pstmt

and pdecl = { pdecl_kind : pdecl_kind; pdecl_loc : location }

type ptranslation_unit = pdecl list
