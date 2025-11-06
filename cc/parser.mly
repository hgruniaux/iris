%{
  open Ast
  open Context

  let mk_unop kind loc = { punop_kind = kind; punop_loc = loc }
  let mk_binop kind loc = { pbinop_kind = kind; pbinop_loc = loc }
  let mk_expr kind loc = { pexpr_kind = kind; pexpr_loc = loc }
  let mk_stmt kind loc = { pstmt_kind = kind; pstmt_loc = loc }
  let mk_decl kind loc = { pdecl_kind = kind; pdecl_loc = loc }

  let with_location value loc = { value; loc }

  let rec declarator_name d = match d.value with
    | Pdeclarator_name name -> name
    | Pdeclarator_paren d -> declarator_name d
    | Pdeclarator_pointer (d, _) -> declarator_name d
    | Pdeclarator_array (d, _, _) -> declarator_name d
    | Pdeclarator_array_static (inner_d, _, _) -> declarator_name inner_d
    | Pdeclarator_array_star (inner_d, _) -> declarator_name inner_d
    | Pdeclarator_function (d, _, _, _) -> declarator_name d

  let rec reinstall_function_context d =
    match d.value with
    | Pdeclarator_function (_, _, _, ctx) ->
      restore_context ctx;
      declare_varname (declarator_name d).id
    | Pdeclarator_paren inner_d -> reinstall_function_context inner_d
    | _ -> assert false
%}

%token EOF
%token <string> IDENT
%token <Z.t * Ast.int_suffix> INTEGER_LITERAL
%token <float * Ast.float_suffix> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> CHAR_LITERAL

(* Special tokens emitted after IDENT to classify the type as either a variable or a type. *)
%token TYPE
%token VARIABLE

%token DOT "."
%token ARROW "->"
%token COLON ":"
%token COMMA ","
%token SEMI ";"
%token QUESTION "?"
%token LPAREN "("
%token RPAREN ")"
%token LSQUARE "["
%token RSQUARE "]"
%token LBRACKET "{"
%token RBRACKET "}"
%token PLUS "+"
%token PLUS_EQ "+="
%token PLUS_PLUS "++"
%token MINUS "-"
%token MINUS_EQ "-="
%token MINUS_MINUS "--"
%token STAR "*"
%token STAR_EQ "*="
%token SLASH "/"
%token SLASH_EQ "/="
%token PERCENT "%"
%token PERCENT_EQ "%="
%token LESS_LESS "<<"
%token LESS_LESS_EQ "<<="
%token GREATER_GREATER ">>"
%token GREATER_GREATER_EQ ">>="
%token AMP "&"
%token AMP_EQ "&="
%token AMP_AMP "&&"
%token PIPE "|"
%token PIPE_EQ "|="
%token PIPE_PIPE "||"
%token CARET "^"
%token CARET_EQ "^="
%token EQ "="
%token EQ_EQ "=="
%token EXCLAIM_EQ "!="
%token LESS "<"
%token LESS_EQ "<="
%token GREATER ">"
%token GREATER_EQ ">="
%token EXCLAIM "!"
%token TILDE "~"
%token ELLIPSIS "..."

%token ALIGNAS "alignas"
%token ALIGNOF "alignof"
%token AUTO "auto"
%token BOOL "bool"
%token BREAK "break"
%token CASE "case"
%token CHAR "char"
%token CONST "const"
%token CONTINUE "continue"
%token DEFAULT "default"
%token DO "do"
%token DOUBLE "double"
%token ELSE "else"
%token ENUM "enum"
%token EXTERN "extern"
%token FALSE "false"
%token FLOAT "float"
%token FOR "for"
%token GOTO "goto"
%token IF "if"
%token INLINE "inline"
%token INT "int"
%token LONG "long"
%token NULLPTR "nullptr"
%token REGISTER "register"
%token RESTRICT "restrict"
%token RETURN "return"
%token SHORT "short"
%token SIGNED "signed"
%token SIZEOF "sizeof"
%token STATIC "static"
%token STATIC_ASSERT "static_assert"
%token STRUCT "struct"
%token SWITCH "switch"
%token THREAD_LOCAL "thread_local"
%token TRUE "true"
%token TYPEDEF "typedef"
%token TYPEOF "typeof"
%token TYPEOF_UNQUAL "typeof_unqual"
%token UNION "union"
%token UNSIGNED "unsigned"
%token VOID "void"
%token VOLATILE "volatile"
%token WHILE "while"
%token NORETURN "_Noreturn"

%token GNU_EXTENSION "__extension__"
%token GNU_ATTRIBUTE "__attribute__"

(* These precedence declarations solve the dangling else conflict. *)
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start <Ast.ptranslation_unit> translation_unit
%type <Ast.ptype> specifier_qualifier_list
%type <Ast.ptype_name> type_name
%type <Ast.pexpr> expression constant_expression
%type <Ast.pstmt> statement

%type <Ast.pabstract_declarator> direct_abstract_declarator

%%

identifier:
  | i=IDENT { { id = i; loc = $loc } }

(* Upon finding an identifier, the lexer emits two tokens. The first token,
   [NAME], indicates that a name has been found; the second token, either [TYPE]
   or [VARIABLE], tells what kind of name this is. The classification is
   performed only when the second token is demanded by the parser. *)
typedef_name:
  | i = identifier TYPE
    { i }

var_name:
  | i = identifier VARIABLE
    { i }

(* [typedef_name_spec] must be declared before [general_identifier], so that the
   reduce/reduce conflict is solved the right way. *)

typedef_name_spec:
  | n=typedef_name
    { Ptype_specifier_typedef_name n }

general_identifier:
  | i=typedef_name
  | i=var_name
    { i }

(* C23 6.5.2 Primary expressions *)
primary_expression:
  | i=var_name { mk_expr (Pexpr_ident i) $sloc }
  | TRUE { mk_expr (Pexpr_bool true) $sloc }
  | FALSE { mk_expr (Pexpr_bool false) $sloc }
  | NULLPTR { mk_expr (Pexpr_nullptr) $sloc }
  | i=INTEGER_LITERAL { mk_expr (Pexpr_int (fst i, snd i)) $sloc }
  | f=FLOAT_LITERAL { mk_expr (Pexpr_float (fst f, snd f)) $sloc }
  | c=CHAR_LITERAL { mk_expr (Pexpr_char c) $sloc }
  | s=string_literal { s }
  | "(" e=expression ")" { mk_expr (Pexpr_paren e) $sloc }
  (* TODO: generic_selection *)

string_literal:
  | s=STRING_LITERAL+ { mk_expr (Pexpr_string s) $sloc }

(* C23 6.5.3 Postfix operators *)
postfix_expression:
  | e=primary_expression { e }
  | e=postfix_expression "[" i=expression "]" { mk_expr (Pexpr_array (e, i)) $sloc }
  | e=postfix_expression "(" args=argument_expression_list ")" { mk_expr (Pexpr_call (e, args)) $sloc }
  | e=postfix_expression "." member=general_identifier { mk_expr (Pexpr_member (e, member)) $sloc }
  | e=postfix_expression "->" member=general_identifier { mk_expr (Pexpr_member_deref (e, member)) $sloc }
  | e=postfix_expression "++" { mk_expr (Pexpr_unary (mk_unop Punop_post_inc $loc($2), e)) $sloc }
  | e=postfix_expression "--" { mk_expr (Pexpr_unary (mk_unop Punop_post_dec $loc($2), e)) $sloc }
  (* TODO: compound_literal *)

argument_expression_list:
  | args=separated_list(",", assignment_expression) { args }

(* C23 6.5.4 Unary operators *)
unary_expression:
  | e=postfix_expression { e }
  | "++" e=unary_expression { mk_expr (Pexpr_unary (mk_unop Punop_pre_inc $loc($1), e)) $sloc }
  | "--" e=unary_expression { mk_expr (Pexpr_unary (mk_unop Punop_pre_dec $loc($1), e)) $sloc }
  | op=unary_operator e=unary_expression { mk_expr (Pexpr_unary (mk_unop op $loc(op), e)) $sloc }
  | SIZEOF e=unary_expression { mk_expr (Pexpr_unary (mk_unop Punop_sizeof $loc($1), e)) $sloc }
  | SIZEOF "(" t=type_name ")" { mk_expr (Pexpr_sizeof t) $sloc }
  | ALIGNOF "(" t=type_name ")" { mk_expr (Pexpr_alignof t) $sloc }
  | GNU_EXTENSION e=unary_expression { e }

%inline unary_operator:
  | "&" {  Punop_addrof }
  | "*" {  Punop_deref }
  | "+" {  Punop_plus }
  | "-" {  Punop_neg }
  | "~" {  Punop_bitnot }
  | "!" {  Punop_lognot }

(* C23 6.5.5 Cast operators *)
cast_expression:
  | e=unary_expression { e }
  | "(" t=type_name ")" e=cast_expression { mk_expr (Pexpr_cast (t, e)) $sloc }

(* C23 6.5.6 Multiplicative operators *)
multiplicative_expression:
  | e=cast_expression { e }
  | lhs=multiplicative_expression op=multiplicative_operator rhs=cast_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline multiplicative_operator:
  | "*" { Pbinop_mul }
  | "/" { Pbinop_div }
  | "%" { Pbinop_mod }

(* C23 6.5.7 Additive operators *)
additive_expression:
  | e=multiplicative_expression { e }
  | lhs=additive_expression op=additive_operator rhs=multiplicative_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline additive_operator:
  | "+" { Pbinop_add }
  | "-" { Pbinop_sub }

(* C23 6.5.8 Bitwise shift operators *)
shift_expression:
  | e=additive_expression { e }
  | lhs=shift_expression op=shift_operator rhs=additive_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline shift_operator:
  | "<<" { Pbinop_shl }
  | ">>" { Pbinop_shr }

(* C23 6.5.9 Relational operators *)
relational_expression:
  | e=shift_expression { e }
  | lhs=relational_expression op=relational_operator rhs=shift_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline relational_operator:
  | "<" { Pbinop_lt }
  | "<=" { Pbinop_le }
  | ">" { Pbinop_gt }
  | ">=" { Pbinop_ge }

(* C23 6.5.10 Equality operators *)
equality_expression:
  | e=relational_expression { e }
  | lhs=equality_expression op=equality_operator rhs=relational_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline equality_operator:
  | "==" { Pbinop_eq }
  | "!=" { Pbinop_ne }

(* C23 6.5.11 Bitwise AND operator *)
and_expression:
  | e=equality_expression { e }
  | lhs=and_expression "&" rhs=equality_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_and $loc($2), lhs, rhs)) $sloc }

(* C23 6.5.12 Bitwise exclusive OR operator *)
exclusive_or_expression:
  | e=and_expression { e }
  | lhs=exclusive_or_expression "^" rhs=and_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_xor $loc($2), lhs, rhs)) $sloc }

(* C23 6.5.13 Bitwise inclusive OR operator *)
inclusive_or_expression:
  | e=exclusive_or_expression { e }
  | lhs=inclusive_or_expression "|" rhs=exclusive_or_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_or $loc($2), lhs, rhs)) $sloc }

(* C23 6.5.14 Logical AND operator *)
logical_and_expression:
  | e=inclusive_or_expression { e }
  | lhs=logical_and_expression "&&" rhs=inclusive_or_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_logand $loc($2), lhs, rhs)) $sloc }

(* C23 6.5.15 Logical OR operator *)
logical_or_expression:
  | e=logical_and_expression { e }
  | lhs=logical_or_expression "||" rhs=logical_and_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_logor $loc($2), lhs, rhs)) $sloc }

(* C23 6.5.16 Conditional operator *)
conditional_expression:
  | e=logical_or_expression { e }
  | cond=logical_or_expression "?" then_expr=expression ":" else_expr=conditional_expression
    { mk_expr (Pexpr_conditional (cond, then_expr, else_expr)) $sloc }

(* C23 6.5.17 Assignment operator *)
assignment_expression:
  | e=conditional_expression { e }
  | lhs=unary_expression op=assignment_operator rhs=assignment_expression
    { mk_expr (Pexpr_binary (mk_binop op $loc(op), lhs, rhs)) $sloc }

%inline assignment_operator:
  | "=" { Pbinop_assign }
  | "+=" { Pbinop_assign_add }
  | "-=" { Pbinop_assign_sub }
  | "*=" { Pbinop_assign_mul }
  | "/=" { Pbinop_assign_div }
  | "%=" { Pbinop_assign_mod }
  | "&=" { Pbinop_assign_and }
  | "|=" { Pbinop_assign_or }
  | "^=" { Pbinop_assign_xor }
  | "<<=" { Pbinop_assign_shl }
  | ">>=" { Pbinop_assign_shr }

(* C23 6.5.18 Comma operator *)
expression:
  | e=assignment_expression { e }
  | lhs=expression "," rhs=assignment_expression
    { mk_expr (Pexpr_binary (mk_binop Pbinop_comma $loc($2), lhs, rhs)) $sloc }

(* C23 6.6 Constant expressions *)
constant_expression:
  | e=conditional_expression { e }

(* C23 6.7 Declarations *)

(* We separate type declarations, which contain an occurrence of ["typedef"], and
   normal declarations, which do not. This makes it possible to distinguish /in
   the grammar/ whether a declaration introduces typedef names or variables in
   the context. *)
declaration:
  | attribute_specifier_sequence t=declaration_specifiers d=init_declarator_list(declarator_varname) ";"
    { mk_decl (Pdecl_variable (t, d)) $sloc }
  | attribute_specifier_sequence t=declaration_specifiers_typedef d=init_declarator_list(declarator_typedefname) ";"
    { mk_decl (Pdecl_typedef (t, d)) $sloc }
  | d=static_assert_declaration { let (e, s) = d in mk_decl (Pdecl_static_assert (e, s)) $sloc }
  | attribute_specifier_sequence_nonempty ";" { mk_decl Pdecl_null $sloc }

(* [declaration_specifier] corresponds to one declaration specifier in the C23
   standard, deprived of "typedef" and of type specifiers. *)
declaration_specifier:
  | s=storage_class_specifier (* deprived of "typedef" *)
    { with_location (Pstorage_class_specifier s) $sloc }
  | s=type_qualifier
    { with_location (Ptype_qualifier s) $sloc }
  | s=function_specifier
    { with_location (Pfunction_specifier s) $sloc }
  | s=alignment_specifier
    { with_location (Ptype_qualifier s) $sloc }

(* [declaration_specifiers] requires that at least one type specifier be
   present, and, if a unique type specifier is present, then no other type
   specifier be present. In other words, one should have either at least one
   nonunique type specifier, or exactly one unique type specifier.

   This is a weaker condition than 6.7.2 2. Encoding this condition in the
   grammar is necessary to disambiguate the example in 6.7.7 6:

     typedef signed int t;
     struct tag {
     unsigned t:4;
     const t:5;
     };

   The first field is a named t, while the second is unnamed of type t.

   [declaration_specifiers] forbids the ["typedef"] keyword. *)
declaration_specifiers:
  | l=list_eq1(type_specifier_unique,    declaration_specifier)
  | l=list_ge1(type_specifier_nonunique, declaration_specifier)
    { l }

(* [declaration_specifiers_typedef] is analogous to [declaration_specifiers],
   but requires the ["typedef"] keyword to be present (exactly once). *)
declaration_specifiers_typedef:
  | l=list_eq1_eq1(typedef, type_specifier_unique,    declaration_specifier)
  | l=list_eq1_ge1(typedef, type_specifier_nonunique, declaration_specifier)
    { l }

%inline typedef:
  | TYPEDEF { with_location (Pstorage_class_specifier Pstorage_class_specifier_typedef) $sloc }

(* The parameter [declarator] in [init_declarator_list] and [init_declarator]
   is instantiated with [declarator_varname] or [declarator_typedefname]. *)

init_declarator_list(declarator):
  | d=separated_list(",", init_declarator(declarator))
    { d }

init_declarator(declarator):
  | d=declarator { d, None }
  | d=declarator "=" e=c_initializer
      { d, Some e }

c_initializer:
  | e=assignment_expression { e }

(* C23 6.7.2 Storage-class specifiers *)
(* [storage_class_specifier] is deprived of ["typedef"] (which receives special treatment). *)
storage_class_specifier:
  | EXTERN { Pstorage_class_specifier_extern }
  | STATIC { Pstorage_class_specifier_static }
  | THREAD_LOCAL { Pstorage_class_specifier_thread_local }
  | AUTO { Pstorage_class_specifier_auto }
  | REGISTER { Pstorage_class_specifier_register }

(* C23 6.7.3.1 General *)
(* A type specifier which can appear together with other type specifiers. *)
type_specifier_nonunique:
  | CHAR { with_location (Ptype_specifier Ptype_specifier_char) $sloc }
  | SHORT { with_location (Ptype_specifier Ptype_specifier_short) $sloc }
  | INT { with_location (Ptype_specifier Ptype_specifier_int) $sloc }
  | LONG { with_location (Ptype_specifier Ptype_specifier_long) $sloc }
  | FLOAT { with_location (Ptype_specifier Ptype_specifier_float) $sloc }
  | DOUBLE { with_location (Ptype_specifier Ptype_specifier_double) $sloc }
  | SIGNED { with_location (Ptype_specifier Ptype_specifier_signed) $sloc }
  | UNSIGNED { with_location (Ptype_specifier Ptype_specifier_unsigned) $sloc }
  (* TODO: complex numbers *)

(* A type specifier which cannot appear together with other type specifiers. *)
type_specifier_unique:
  | VOID { with_location (Ptype_specifier Ptype_specifier_void) $sloc }
  | BOOL { with_location (Ptype_specifier Ptype_specifier_bool) $sloc }
  | t=struct_or_union_specifier { with_location (Ptype_specifier t) $sloc }
  | t=enum_specifier { with_location (Ptype_specifier t) $sloc }
  | t=typedef_name_spec { with_location (Ptype_specifier t) $sloc }
  | t=typeof_specifier { with_location (Ptype_specifier t) $sloc }

(* [specifier_qualifier_list] is as in the standard, except it also encodes the
   same constraint as [declaration_specifiers] (see above). *)
specifier_qualifier_list:
  | l=list_eq1(type_specifier_unique, type_qualifier_or_alignment_specifier)
  | l=list_ge1(type_specifier_nonunique, type_qualifier_or_alignment_specifier)
    { l }

type_qualifier_or_alignment_specifier:
  | q=type_qualifier { with_location (Ptype_qualifier q) $sloc }
  | q=alignment_specifier { with_location (Ptype_qualifier q) $sloc }

(* C23 6.7.3.2 Structure and union specifiers *)
struct_or_union_specifier:
  | STRUCT attribute_specifier_sequence name=general_identifier? "{" members=member_declaration* "}"
    { Ptype_specifier_struct (name, Some members) }
  | STRUCT attribute_specifier_sequence name=general_identifier?
    { Ptype_specifier_struct (name, None) }
  | UNION attribute_specifier_sequence name=general_identifier? "{" members=member_declaration* "}"
    { Ptype_specifier_union (name, Some members) }
  | UNION attribute_specifier_sequence name=general_identifier?
    { Ptype_specifier_union (name, None) }

member_declaration:
  | GNU_EXTENSION? attribute_specifier_sequence t=specifier_qualifier_list decls=separated_list(",", member_declarator) ";"
    { with_location (Pstruct_member_field (t, decls)) $sloc }
  | GNU_EXTENSION? d=static_assert_declaration
    {
      let (e, s) = d in
      with_location (Pstruct_member_static_assert (e, s)) $sloc
    }

member_declarator:
  | d=declarator_varname { d }
  (* TODO: bitfields *)

(* C23 6.7.3.3 Enumeration specifiers *)
enum_specifier:
  | "enum" attribute_specifier_sequence n=general_identifier? t=enum_type_specifier? "{" m=separated_nonempty_list_option_trailing(",", enumerator) ","? "}"
    { Ptype_specifier_enum (n, t, Some m)  }
  | "enum" attribute_specifier_sequence n=general_identifier? t=enum_type_specifier?
    { Ptype_specifier_enum (n, t, None) }

enum_type_specifier:
  | ":" t=specifier_qualifier_list { t }

enumerator:
  | i=enumeration_constant
    { declare_varname i.id; (i, None) }
  | i=enumeration_constant "=" e=constant_expression
    { declare_varname i.id; (i, Some e) }

enumeration_constant: i=general_identifier { i }

(* C23 6.7.3.6 Typeof specifiers *)
typeof_specifier:
  | TYPEOF "(" e=expression ")" { Ptype_specifier_typeof_expr (e, false) }
  | TYPEOF "(" t=type_name ")" { Ptype_specifier_typeof_type (t, false) }
  | TYPEOF_UNQUAL "(" e=expression ")" { Ptype_specifier_typeof_expr (e, true) }
  | TYPEOF_UNQUAL "(" t=type_name ")" { Ptype_specifier_typeof_type (t, true) }

(* C23 6.7.4 Type qualifiers *)
type_qualifier:
  | CONST    { Ptype_qualifier_const }
  | VOLATILE { Ptype_qualifier_volatile }
  | RESTRICT { Ptype_qualifier_restrict }
  (* TODO: _Atomic *)

(* C23 6.7.5 Function specifiers *)
function_specifier:
  | INLINE   { Pfunction_specifier_inline }
  | NORETURN { Pfunction_specifier_noreturn }

(* C23 6.7.6 Alignment specifier *)
alignment_specifier:
  | ALIGNAS "(" e=constant_expression ")"
    { Ptype_qualifier_alignas_expr e }
  | ALIGNAS "(" t=type_name ")"
    { Ptype_qualifier_alignas_type t }

(* C23 6.7.7 Declarators *)
declarator:
  | d=direct_declarator { d }
  | "*" attribute_specifier_sequence quals=type_qualifier* d=declarator
    { with_location (Pdeclarator_pointer (d, quals)) $sloc }

direct_declarator:
  | name=general_identifier attribute_specifier_sequence { with_location (Pdeclarator_name name) $sloc }
  | "(" save_context d=declarator ")" { with_location (Pdeclarator_paren d) $sloc }
  | d=array_declarator attribute_specifier_sequence { d }
  | d=function_declarator attribute_specifier_sequence { d }

array_declarator:
  | d=direct_declarator "[" quals=type_qualifier* s=assignment_expression? "]"
    { with_location (Pdeclarator_array (d, quals, s)) $sloc }
  | d=direct_declarator "[" STATIC quals=type_qualifier* s=assignment_expression "]"
    { with_location (Pdeclarator_array_static (d, quals, s)) $sloc }
  | d=direct_declarator "[" quals=type_qualifier+ STATIC s=assignment_expression "]"
    { with_location (Pdeclarator_array_static (d, quals, s)) $sloc }
  | d=direct_declarator "[" quals=type_qualifier* "*" "]"
    { with_location (Pdeclarator_array_star (d, quals)) $sloc }

function_declarator:
  | d=direct_declarator "(" params=scoped(parameter_type_list) ")"
    {
      let (params, ellipsis, ctx) = params in
      with_location (Pdeclarator_function (d, params, ellipsis, ctx) ) $sloc
    }

parameter_type_list:
  | /* empty */ ctx = save_context { [], false, ctx }
  | "..." ctx = save_context { [], true, ctx }
  | p=parameter_list ellipsis=ioption("," "..." {}) ctx = save_context
      { p, Option.is_some ellipsis, ctx }

parameter_list:
  | d=parameter_declaration { [d] }
  | l=parameter_list "," d=parameter_declaration { l @ [d] }

parameter_declaration:
  | attribute_specifier_sequence s=declaration_specifiers d=declarator_varname
    { with_location (Pparameter_named (s, d)) $sloc }
  | attribute_specifier_sequence s=declaration_specifiers d=abstract_declarator?
    { with_location (Pparameter_unnamed (s, d)) $sloc }

(* [declarator_varname] and [declarator_typedefname] are like [declarator]. In
   addition, they have the side effect of introducing the declared identifier as
   a new variable or typedef name in the current context. *)

declarator_typedefname:
  | d=declarator
    {
      let name = declarator_name d in
      declare_typedefname name.id;
      d
    }

declarator_varname:
  | d=declarator
    {
      let name = declarator_name d in
      declare_varname name.id;
      d
    }

(* C23 6.7.8 Type names *)
type_name:
  | t=specifier_qualifier_list d=abstract_declarator?
    { (t, d) }

abstract_declarator:
  | d=direct_abstract_declarator { d }
  | "*" attribute_specifier_sequence quals=type_qualifier* d=abstract_declarator?
    { Pabstract_declarator_pointer (d, quals) }

direct_abstract_declarator:
  | "(" save_context d=abstract_declarator ")" { Pabstract_declarator_paren d }
  | d=array_abstract_declarator attribute_specifier_sequence { d }
  | d=function_abstract_declarator attribute_specifier_sequence { d }

array_abstract_declarator:
  | d=direct_abstract_declarator "[" quals=type_qualifier* s=assignment_expression? "]"
    { Pabstract_declarator_array (d, quals, s) }
  | d=direct_abstract_declarator "[" STATIC quals=type_qualifier* s=assignment_expression "]"
    { Pabstract_declarator_array_static (d, quals, s) }
  | d=direct_abstract_declarator "[" quals=type_qualifier+ STATIC s=assignment_expression "]"
    { Pabstract_declarator_array_static (d, quals, s) }
  | d=direct_abstract_declarator "[" quals=type_qualifier* "*" "]"
    { Pabstract_declarator_array_star (d, quals) }

function_abstract_declarator:
  | d=ioption(direct_abstract_declarator) "(" params=scoped(parameter_type_list) ")"
    {
      let (params, ellipsis, _) = params in
      Pabstract_declarator_function (d, params, ellipsis)
    }

(* C23 6.7.12 Static assertions *)
static_assert_declaration:
  | STATIC_ASSERT "(" e=constant_expression ")" ";" { e, None }
  | STATIC_ASSERT "(" e=constant_expression "," s=string_literal ")" ";" { e, Some s }

(* C23 6.7.13 Attributes *)
attribute_specifier_sequence:
  | attrs=attribute_specifier* { ignore attrs }

attribute_specifier_sequence_nonempty:
  | attrs=attribute_specifier+ { ignore attrs }

attribute_specifier:
  | GNU_ATTRIBUTE "(" "(" expression ")" ")" {}

(* C23 6.8.1 General *)
statement:
  | s=labeled_statement { s }
  | s=unlabeled_statement { s }

unlabeled_statement:
  | s=expression_statement { s }
  | attribute_specifier_sequence s=scoped(primary_block) { s }
  | attribute_specifier_sequence s=jump_statement { s }

primary_block:
  | s=compound_statement { s }
  | s=selection_statement { s }
  | s=iteration_statement { s }

secondary_block:
  | s=scoped(statement) { s }

(* C23 6.8.2 Labeled statements *)
labeled_statement:
  | attribute_specifier_sequence l=general_identifier ":" s=statement { mk_stmt (Pstmt_label (l, s)) $sloc }
  | attribute_specifier_sequence CASE e=constant_expression ":" s=statement { mk_stmt (Pstmt_case (e, s)) $sloc }
  | attribute_specifier_sequence DEFAULT ":" s=statement { mk_stmt (Pstmt_default s) $sloc }

(* C23 6.8.3 Compound statement *)
compound_statement:
  | "{" items=block_item* "}" { mk_stmt (Pstmt_compound items) $sloc }

block_item:
  | d=declaration { mk_stmt (Pstmt_decl d) $sloc }
  | s=statement { s }

(* C23 6.8.4 Expression and null statement *)
expression_statement:
  | attribute_specifier_sequence e=expression ";" { mk_stmt (Pstmt_expr e) $sloc }
  | ";" { mk_stmt Pstmt_null $sloc }

(* C23 6.8.5 Selection statements *)
selection_statement:
  | IF "(" cond=expression ")" then_stmt=secondary_block ELSE else_stmt=secondary_block
    { mk_stmt (Pstmt_if (cond, then_stmt, Some else_stmt)) $sloc }
  | IF "(" cond=expression ")" then_stmt=secondary_block %prec LOWER_THAN_ELSE
    { mk_stmt (Pstmt_if (cond, then_stmt, None)) $sloc }
  | SWITCH "(" cond=expression ")" body=secondary_block
    { mk_stmt (Pstmt_switch (cond, body)) $sloc }

(* C23 6.8.6 Iteration statements *)
iteration_statement:
  | WHILE "(" cond=expression ")" body=secondary_block
    { mk_stmt (Pstmt_while (cond, body)) $sloc }
  | DO body=secondary_block WHILE "(" cond=expression ")" ";"
    { mk_stmt (Pstmt_do (body, cond)) $sloc }
  | FOR "(" init=expression? ";" cond=expression? ";" update=expression? ")" body=secondary_block
    {
      let init_stmt = Option.map (fun e -> mk_stmt (Pstmt_expr e) $sloc) init in
      mk_stmt (Pstmt_for (init_stmt, cond, update, body)) $sloc
    }
  | FOR "(" init=declaration cond=expression? ";" update=expression? ")" body=secondary_block
    {
      let init_stmt = mk_stmt (Pstmt_decl init) $sloc in
      mk_stmt (Pstmt_for (Some init_stmt, cond, update, body)) $sloc
    }

(* C23 6.8.7 Jump statements *)
jump_statement:
  | RETURN e=expression ";" { mk_stmt (Pstmt_return (Some e)) $sloc }
  | RETURN ";" { mk_stmt (Pstmt_return None) $sloc }
  | BREAK ";" { mk_stmt Pstmt_break $sloc }
  | CONTINUE ";" { mk_stmt Pstmt_continue $sloc }
  | GOTO l=general_identifier ";" { mk_stmt (Pstmt_goto l) $sloc }

(* C23 6.9 External definitions *)
translation_unit:
  | decls=external_declaration* EOF { decls }

external_declaration:
  | GNU_EXTENSION? d=declaration { d }
  | GNU_EXTENSION? d=function_definition { d }

(* C23 6.9.2 Function definitions *)
function_definition1:
  | attribute_specifier_sequence t=declaration_specifiers d=declarator_varname
    { let ctx = save_context () in
      reinstall_function_context d;
      ctx, t, d }

function_definition:
  | signature=function_definition1 body=compound_statement
    {
      let (ctx, t, d) = signature in
      restore_context ctx;
      mk_decl (Pdecl_function_definition (t, d, body)) $sloc
    }

save_context: { save_context () }

scoped(X): ctx=save_context  x=X
  { restore_context ctx; x }

(* A list of A's and B's that contains exactly one A: *)
list_eq1(A, B):
  | hd=A tl=B* { hd :: tl }
  | hd=B tl=list_eq1(A, B) { hd :: tl }

(* A list of A's and B's that contains at least one A: *)
list_ge1(A, B):
  | hd=A tl=B* { hd :: tl }
  | hd=A tl=list_ge1(A, B) { hd :: tl }
  | hd=B tl=list_ge1(A, B) { hd :: tl }

(* A list of A's, B's and C's that contains exactly one A and exactly one B: *)
list_eq1_eq1(A, B, C):
  | hd=A tl=list_eq1(B, C) { hd :: tl }
  | hd=B tl=list_eq1(A, C) { hd :: tl }
  | hd=C tl=list_eq1_eq1(A, B, C) { hd :: tl }

(* A list of A's, B's and C's that contains exactly one A and at least one B: *)
list_eq1_ge1(A, B, C):
  | hd=A tl=list_ge1(B, C) { hd :: tl }
  | hd=B tl=list_eq1(A, C) { hd :: tl }
  | hd=B tl=list_eq1_ge1(A, B, C) { hd :: tl }
  | hd=C tl=list_eq1_ge1(A, B, C) { hd :: tl }

(* A separated non-empty list of X's that may be followed by a trailing separator: *)
separated_nonempty_list_option_trailing(sep, X):
  | x=X { [x] }
  | x=X sep xs=separated_nonempty_list(sep, X); { [x] @ xs }
  | x=X sep { [x] }
