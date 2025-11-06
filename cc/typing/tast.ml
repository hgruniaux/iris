type identifier = Ast.identifier
type location = Ast.location
type binop = Ast.binop
type unop = Ast.unop

type cast_kind =
  | Tcast_invalid  (** Invalid cast. *)
  | Tcast_noop  (** Cast between two compatible types. *)
  | Tcast_void  (** Cast to void. The expression value is discarded. *)
  | Tcast_int2bool
  | Tcast_int2int
  | Tcast_int2float
  | Tcast_int2pointer
  | Tcast_float2bool
  | Tcast_float2int
  | Tcast_float2float
  | Tcast_pointer2bool
  | Tcast_pointer2pointer
  | Tcast_pointer2int
  | Tcast_function2pointer  (** Function to function pointer. *)
  | Tcast_array2pointer  (** Array to pointer. *)
  | Tcast_null2pointer  (** Null pointer constant to pointer. *)
  | Tcast_null2function  (** Null pointer constant to function pointer. *)
  | Tcast_lvalue2rvalue  (** Lvalue to rvalue conversion. *)

type linkage = Tlinkage_none | Tlinkage_internal | Tlinkage_external
type storage = Tstorage_auto | Tstorage_static

type ttype =
  | Ttyp_void
  | Ttyp_bool
  | Ttyp_char
  | Ttyp_signed_char
  | Ttyp_unsigned_char
  | Ttyp_signed_short
  | Ttyp_unsigned_short
  | Ttyp_signed_int
  | Ttyp_unsigned_int
  | Ttyp_signed_long
  | Ttyp_unsigned_long
  | Ttyp_signed_long_long
  | Ttyp_unsigned_long_long
  | Ttyp_float
  | Ttyp_double
  | Ttyp_long_double
  | Ttyp_ptr of ttype
  | Ttyp_array of ttype * int option
  | Ttyp_enum of tenum_declaration
  | Ttyp_struct of tstruct_declaration
  | Ttyp_function of ttype * ttype list * bool
  | Ttyp_typedef of ttypedef_declaration

and texpr_kind =
  (* C23 6.5.2 Primary expressions *)
  | Texpr_bool of bool
  | Texpr_int of Z.t
  | Texpr_float of float
  | Texpr_string of string
  | Texpr_decl of tvalue_decl
  | Texpr_paren of texpr
  (* C23 6.5.3 Postfix expressions *)
  | Texpr_array of texpr * texpr
  | Texpr_call of texpr * texpr list
  | Texpr_member of texpr * tstruct_field
  | Texpr_member_deref of texpr * tstruct_field
  (* C23 6.5.4 Unary operators *)
  | Texpr_unary of unop * texpr
  | Texpr_sizeof of ttype
  | Texpr_alignof of ttype
  (* C23 6.5.5 Cast operators *)
  | Texpr_cast of ttype * texpr * cast_kind
  (* C23 6.5.16 Conditional operator *)
  | Texpr_conditional of texpr * texpr * texpr
  (* C23 6.5.6 - 6.5.15 and 6.5.17 - 6.5.18 *)
  | Texpr_binary of binop * texpr * texpr
  (* Others, internal expressions. *)
  | Texpr_implicit_cast of
      ttype * texpr * cast_kind (* automatically inserted by the typer *)

and texpr = {
  texpr_kind : texpr_kind;
  texpr_type : ttype;
  texpr_loc : location;
  texpr_is_lvalue : bool;
}

and tstmt_kind =
  | Tstmt_decl of tdecl
  (* C23 6.8.2 Labeled statements *)
  | Tstmt_label of tlabel_declaration * tstmt
  | Tstmt_case of texpr * tstmt
  | Tstmt_default of tstmt
  (* C23 6.8.3 Compound statement *)
  | Tstmt_compound of tstmt list
  (* C23 6.8.4 Expression and null statements *)
  | Tstmt_null
  | Tstmt_expr of texpr
  (* C23 6.8.5 Selection statements *)
  | Tstmt_if of texpr * tstmt * tstmt option
  | Tstmt_switch of texpr * tswitch_labels * tstmt
  (* C23 6.8.6 Iteration statements *)
  | Tstmt_while of texpr * tstmt
  | Tstmt_do of tstmt * texpr
  | Tstmt_for of tstmt option * texpr option * tstmt option * tstmt
  (* 6.8.7 Jump statements *)
  | Tstmt_goto of tlabel_declaration
  | Tstmt_return of texpr option
  | Tstmt_break
  | Tstmt_continue

and tstmt = { tstmt_kind : tstmt_kind; tstmt_loc : location }
and tlabel_declaration = { tlabel_name : identifier; tlabel_loc : location }

and tswitch_labels = {
  mutable tswitch_default : tstmt option;
  mutable tswitch_cases : (Z.t * tstmt) list;
}

and tstruct_declaration = {
  tstruct_name : identifier option;
  tstruct_is_union : bool;
  mutable tstruct_fields : tstruct_field list option;
}

and tstruct_field = {
  tstruct_field_name : identifier;
  tstruct_field_type : ttype;
}

and tenum_declaration = {
  tenum_name : identifier option;
  tenum_underlying_type : ttype;
  mutable tenum_enumerators : tenumerator_declaration list option;
}

and tenumerator_declaration = {
  tenumerator_name : identifier;
  tenumerator_value : Z.t;
}

and ttypedef_declaration = { ttypedef_name : identifier; ttypedef_type : ttype }

and tfunction_declaration = {
  tfun_name : identifier;
  tfun_type : ttype;
  tfun_return_type : ttype;
  tfun_params : tvalue_decl list;
  mutable tfun_body : tstmt option;
}

and tobject_declaration = {
  tvar_name : identifier option;
  tvar_storage : storage;
  tvar_type : ttype;
  tvar_is_param : bool;
  tvar_init : texpr option;
  tvar_loc : location;
}

and tvalue_decl_kind =
  | Tvalue_decl_object of tobject_declaration
  | Tvalue_decl_function of tfunction_declaration

and tvalue_decl = {
  tvalue_decl_name : identifier option;
  tvalue_decl_kind : tvalue_decl_kind;
  tvalue_decl_linkage : linkage;
  tvalue_decl_type : ttype;
  tvalue_decl_loc : location;
}

and tdecl_kind =
  | Tdecl_typedef of ttypedef_declaration
  | Tdecl_value of tvalue_decl
  | Tdecl_decls of tdecl list

and tdecl = { tdecl_kind : tdecl_kind; tdecl_loc : location }
and ttranslation_unit = tdecl list

let mk_tstmt kind loc = { tstmt_kind = kind; tstmt_loc = loc }
let mk_tdecl kind loc = { tdecl_kind = kind; tdecl_loc = loc }

let mk_texpr ?(is_lvalue = false) kind typ loc =
  {
    texpr_kind = kind;
    texpr_type = typ;
    texpr_loc = loc;
    texpr_is_lvalue = is_lvalue;
  }

let rec ignore_parens expr =
  match expr.texpr_kind with
  | Texpr_paren inner_expr -> ignore_parens inner_expr
  | _ -> expr
