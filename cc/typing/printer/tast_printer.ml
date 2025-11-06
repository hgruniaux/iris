(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Ast
open Tast
open Format
open Codegen_type
open LibIris

type node_category = Declaration | Expression | Statement

let pp_with_color pp color fmt v =
  fprintf fmt "\x1b[%sm" color;
  pp fmt v;
  fprintf fmt "\x1b[0m"

let declaration_color = "32" (* Green *)
let expression_color = "34" (* Blue *)
let statement_color = "35" (* Magenta *)

(** Used for identifiers (variables, functions, etc.) in the source code. *)
let identifier_color = "36" (* Cyan *)

(** Used for keywords in the C language, in types for example. *)
let keyword_color = "31" (* Blue *)

(** Used for literals and constant values. *)
let constant_color = "32" (* Green *)

(** Used for locations in the source code. *)
let location_color = "33" (* Yellow *)

let pp_keyword fmt v = pp_with_color pp_print_string keyword_color fmt v
let pp_constant pp fmt v = pp_with_color pp constant_color fmt v

let pp_identifier fmt name_opt =
  match name_opt with
  | None -> pp_with_color pp_print_string identifier_color fmt "<anonymous>"
  | Some name -> pp_with_color pp_print_string identifier_color fmt name.id

let pp_location fmt (b, e) =
  let aux fmt (b, e) =
    if b = Lexing.dummy_pos || e = Lexing.dummy_pos then
      pp_print_string fmt "<unknown>"
    else
      let start_line = b.pos_lnum in
      let end_line = e.pos_lnum in
      let start_column = b.pos_cnum - b.pos_bol + 1 in
      let end_column = e.pos_cnum - e.pos_bol + 1 in
      if start_line = end_line then
        fprintf fmt "<%d:%d-%d>" start_line start_column end_column
      else
        fprintf fmt "<%d:%d-%d:%d>" start_line start_column end_line end_column
  in
  pp_with_color aux location_color fmt (b, e)

let pp_type fmt t =
  let rec aux fmt t =
    match t with
    (* --- Base types (unchanged) --- *)
    | Ttyp_void -> pp_keyword fmt "void"
    | Ttyp_bool -> pp_keyword fmt "bool"
    | Ttyp_char -> pp_keyword fmt "char"
    | Ttyp_signed_char -> pp_keyword fmt "signed char"
    | Ttyp_unsigned_char -> pp_keyword fmt "unsigned char"
    | Ttyp_signed_short -> pp_keyword fmt "short"
    | Ttyp_unsigned_short -> pp_keyword fmt "unsigned short"
    | Ttyp_signed_int -> pp_keyword fmt "int"
    | Ttyp_unsigned_int -> pp_keyword fmt "unsigned int"
    | Ttyp_signed_long -> pp_keyword fmt "long"
    | Ttyp_unsigned_long -> pp_keyword fmt "unsigned long"
    | Ttyp_signed_long_long -> pp_keyword fmt "long long"
    | Ttyp_unsigned_long_long -> pp_keyword fmt "unsigned long long"
    | Ttyp_float -> pp_keyword fmt "float"
    | Ttyp_double -> pp_keyword fmt "double"
    | Ttyp_long_double -> pp_keyword fmt "long double"
    | Ttyp_struct _ -> fprintf fmt "<struct/union type>"
    | Ttyp_enum _ -> fprintf fmt "<enum type>"
    | Ttyp_typedef typedef_decl ->
        fprintf fmt "%a (aka %a)" pp_identifier
          (Some typedef_decl.ttypedef_name) aux typedef_decl.ttypedef_type
    | Ttyp_ptr typ -> (
        let rec peel_pointers t stars =
          match t with
          | Ttyp_ptr inner -> peel_pointers inner (stars + 1)
          | _ -> (t, stars)
        in

        let pp_stars fmt n =
          for _ = 1 to n do
            fprintf fmt "*"
          done
        in

        (* Peel off all pointers to find the core type and the number of pointers. *)
        (* We start with 1 star because we're inside the Ttyp_ptr case *)
        let core_type, num_stars = peel_pointers typ 1 in

        match core_type with
        | Ttyp_array (inner_typ, size_opt) -> (
            match size_opt with
            | None -> fprintf fmt "%a (%a)[]" aux inner_typ pp_stars num_stars
            | Some size ->
                fprintf fmt "%a (%a)[%a]" aux inner_typ pp_stars num_stars
                  (pp_constant pp_print_int) size)
        | Ttyp_function (return_type, param_types, is_variadic) ->
            fprintf fmt "%a (%a)(%a)" aux return_type pp_stars num_stars
              pp_function_params (param_types, is_variadic)
        | _ -> fprintf fmt "%a%a" aux core_type pp_stars num_stars)
    | Ttyp_array (typ, size_opt) -> (
        (* This is only hit for non-pointer arrays *)
        match size_opt with
        | None -> fprintf fmt "%a[]" aux typ
        | Some size ->
            fprintf fmt "%a[%a]" aux typ (pp_constant pp_print_int) size)
    | Ttyp_function (return_type, param_types, is_variadic) ->
        (* This is only hit for non-pointer function types *)
        fprintf fmt "%a (%a)" aux return_type pp_function_params
          (param_types, is_variadic)
  and pp_function_params fmt (param_types, is_variadic) =
    let rec pp_params fmt param_types =
      match param_types with
      | [] -> ()
      | [ last_param ] -> aux fmt last_param
      | param :: rest ->
          fprintf fmt "%a, " aux param;
          pp_params fmt rest
    in
    pp_params fmt param_types;
    if is_variadic then
      if param_types = [] then fprintf fmt "..." else fprintf fmt ", ..."
  in

  fprintf fmt "'%a'" aux t

let pp_binop fmt op =
  let aux fmt op =
    match op.pbinop_kind with
    | Pbinop_add -> fprintf fmt "+"
    | Pbinop_sub -> fprintf fmt "-"
    | Pbinop_mul -> fprintf fmt "*"
    | Pbinop_div -> fprintf fmt "/"
    | Pbinop_mod -> fprintf fmt "%%"
    | Pbinop_and -> fprintf fmt "&"
    | Pbinop_or -> fprintf fmt "|"
    | Pbinop_xor -> fprintf fmt "^"
    | Pbinop_shl -> fprintf fmt "<<"
    | Pbinop_shr -> fprintf fmt ">>"
    | Pbinop_eq -> fprintf fmt "=="
    | Pbinop_ne -> fprintf fmt "!="
    | Pbinop_lt -> fprintf fmt "<"
    | Pbinop_le -> fprintf fmt "<="
    | Pbinop_gt -> fprintf fmt ">"
    | Pbinop_ge -> fprintf fmt ">="
    | Pbinop_logand -> fprintf fmt "&&"
    | Pbinop_logor -> fprintf fmt "||"
    | Pbinop_assign -> fprintf fmt "="
    | Pbinop_assign_add -> fprintf fmt "+="
    | Pbinop_assign_sub -> fprintf fmt "-="
    | Pbinop_assign_mul -> fprintf fmt "*="
    | Pbinop_assign_div -> fprintf fmt "/="
    | Pbinop_assign_mod -> fprintf fmt "%%="
    | Pbinop_assign_and -> fprintf fmt "&="
    | Pbinop_assign_or -> fprintf fmt "|="
    | Pbinop_assign_xor -> fprintf fmt "^="
    | Pbinop_assign_shl -> fprintf fmt "<<="
    | Pbinop_assign_shr -> fprintf fmt ">>="
    | Pbinop_comma -> fprintf fmt "comma"
  in
  fprintf fmt "'%a'" aux op

let pp_unop fmt op =
  let aux fmt op =
    match op.punop_kind with
    | Punop_plus -> fprintf fmt "+"
    | Punop_neg -> fprintf fmt "-"
    | Punop_lognot -> fprintf fmt "!"
    | Punop_bitnot -> fprintf fmt "~"
    | Punop_pre_inc -> fprintf fmt "++ (pre)"
    | Punop_pre_dec -> fprintf fmt "-- (pre)"
    | Punop_post_inc -> fprintf fmt "++ (post)"
    | Punop_post_dec -> fprintf fmt "-- (post)"
    | Punop_addrof -> fprintf fmt "&"
    | Punop_deref -> fprintf fmt "*"
    | Punop_sizeof -> fprintf fmt "sizeof"
  in
  fprintf fmt "'%a'" aux op

let pp_castkind fmt op =
  let aux fmt op =
    match op with
    | Tcast_invalid -> pp_keyword fmt "Invalid"
    | Tcast_noop -> pp_keyword fmt "NoOp"
    | Tcast_void -> pp_keyword fmt "Void"
    | Tcast_lvalue2rvalue -> pp_keyword fmt "Lvalue2Rvalue"
    | Tcast_int2bool -> pp_keyword fmt "Int2Bool"
    | Tcast_int2int -> pp_keyword fmt "Int2Int"
    | Tcast_int2float -> pp_keyword fmt "Int2Float"
    | Tcast_float2bool -> pp_keyword fmt "Float2Bool"
    | Tcast_float2int -> pp_keyword fmt "Float2Int"
    | Tcast_float2float -> pp_keyword fmt "Float2Float"
    | Tcast_pointer2bool -> pp_keyword fmt "Pointer2Bool"
    | Tcast_pointer2int -> pp_keyword fmt "Pointer2Int"
    | Tcast_int2pointer -> pp_keyword fmt "Int2Pointer"
    | Tcast_pointer2pointer -> pp_keyword fmt "Pointer2Pointer"
    | Tcast_array2pointer -> pp_keyword fmt "Array2Pointer"
    | Tcast_function2pointer -> pp_keyword fmt "Function2Pointer"
    | Tcast_null2function -> pp_keyword fmt "Null2Function"
    | Tcast_null2pointer -> pp_keyword fmt "Null2Pointer"
  in
  fprintf fmt "<%a>" aux op

type node_info = {
  node_name : string;
  node_category : node_category;
  node_loc : Ast.location;
  node_extra : Format.formatter -> unit -> unit;
}

let node ?(extra = fun _ () -> ()) node_name node_category loc children =
  Tree_printer.Node
    ({ node_name; node_category; node_loc = loc; node_extra = extra }, children)

let decl_node class_name decl_name decl_loc decl_type children =
  let extra fmt () =
    fprintf fmt "%a %a" pp_identifier decl_name pp_type decl_type
  in
  node ~extra class_name Declaration decl_loc children

let expr_node ?(extra = fun _ () -> ()) class_name expr children =
  let expr_extra fmt () =
    fprintf fmt "%a %a" pp_type expr.texpr_type extra ();
    if expr.texpr_is_lvalue then fprintf fmt " lvalue"
  in
  node ~extra:expr_extra class_name Expression expr.texpr_loc children

let stmt_node ?(extra = fun _ () -> ()) class_name stmt children =
  node ~extra class_name Statement stmt.tstmt_loc children

let print_node_info fmt node_info =
  let node_color =
    match node_info.node_category with
    | Declaration -> declaration_color
    | Expression -> expression_color
    | Statement -> statement_color
  in
  fprintf fmt "%a %a %a"
    (pp_with_color pp_print_string node_color)
    node_info.node_name pp_location node_info.node_loc node_info.node_extra ()

let rec expr_tree expr =
  let expr_constant_node const_name expr pp v =
    expr_node const_name expr ~extra:(fun fmt () -> pp_constant pp fmt v) []
  in

  let expr_cast_node const_name expr target_type sub_expr cast_kind =
    expr_node const_name expr
      ~extra:(fun fmt () ->
        if expr.texpr_type <> target_type then (
          pp_type fmt target_type;
          fprintf fmt " ");
        pp_castkind fmt cast_kind)
      [ expr_tree sub_expr ]
  in

  let expr_sizeof_node expr typ children =
    let extra fmt () =
      let ir_type = codegen_type typ in
      let size = Machine_info.size_of ir_type in
      fprintf fmt "%a %a" pp_type typ (pp_constant pp_print_int) size
    in
    expr_node "SizeofExpr" expr ~extra children
  in

  let expr_alignof_node expr typ =
    let extra fmt () =
      let ir_type = codegen_type typ in
      let align = Machine_info.align_of ir_type in
      fprintf fmt "%a %a" pp_type typ (pp_constant pp_print_int) align
    in
    expr_node "AlignofExpr" expr ~extra []
  in

  match expr.texpr_kind with
  | Texpr_bool b -> expr_constant_node "BoolExpr" expr pp_print_bool b
  | Texpr_int i -> expr_constant_node "IntExpr" expr Z.pp_print i
  | Texpr_float f -> expr_constant_node "FloatExpr" expr pp_print_float f
  | Texpr_string s ->
      expr_constant_node "StringExpr" expr (fun fmt s -> fprintf fmt "%S" s) s
  | Texpr_paren expr_inner ->
      expr_node "ParenExpr" expr [ expr_tree expr_inner ]
  | Texpr_call (callee, args) ->
      expr_node "CallExpr" expr (expr_tree callee :: List.map expr_tree args)
  | Texpr_unary (op, operand) -> (
      match op.punop_kind with
      | Punop_sizeof ->
          expr_sizeof_node expr operand.texpr_type [ expr_tree operand ]
      | _ ->
          let extra fmt () = pp_unop fmt op in
          expr_node "UnaryExpr" expr ~extra [ expr_tree operand ])
  | Texpr_binary (op, left, right) ->
      let extra fmt () = pp_binop fmt op in
      expr_node "BinaryExpr" expr ~extra [ expr_tree left; expr_tree right ]
  | Texpr_conditional (cond, then_expr, else_expr) ->
      expr_node "ConditionalExpr" expr
        [ expr_tree cond; expr_tree then_expr; expr_tree else_expr ]
  | Texpr_cast (target_type, sub_expr, cast_kind) ->
      expr_cast_node "CastExpr" expr target_type sub_expr cast_kind
  | Texpr_implicit_cast (target_type, sub_expr, cast_kind) ->
      expr_cast_node "ImplicitCastExpr" expr target_type sub_expr cast_kind
  | Texpr_sizeof typ -> expr_sizeof_node expr typ []
  | Texpr_alignof typ -> expr_alignof_node expr typ
  | Texpr_member (base, member_name) ->
      let extra fmt () =
        pp_identifier fmt (Some member_name.tstruct_field_name)
      in
      expr_node "MemberExpr" expr ~extra [ expr_tree base ]
  | Texpr_member_deref (base, member_name) ->
      let extra fmt () =
        pp_identifier fmt (Some member_name.tstruct_field_name)
      in
      expr_node "MemberDerefExpr" expr ~extra [ expr_tree base ]
  | Texpr_decl decl ->
      let extra fmt () = pp_identifier fmt decl.tvalue_decl_name in
      expr_node "DeclRefExpr" expr ~extra []
  | Texpr_array (base, index) ->
      expr_node "ArrayExpr" expr [ expr_tree base; expr_tree index ]

let rec stmt_tree stmt =
  match stmt.tstmt_kind with
  | Tstmt_null -> stmt_node "NullStmt" stmt []
  | Tstmt_expr expr -> expr_tree expr
  | Tstmt_decl decl -> decl_tree decl
  | Tstmt_default child -> stmt_node "DefaultStmt" stmt [ stmt_tree child ]
  | Tstmt_case (expr, child) ->
      stmt_node "CaseStmt" stmt [ expr_tree expr; stmt_tree child ]
  | Tstmt_label (name, child) ->
      let extra fmt () = pp_identifier fmt (Some name.tlabel_name) in
      stmt_node ~extra "LabelStmt" stmt [ stmt_tree child ]
  | Tstmt_compound stmts ->
      stmt_node "CompoundStmt" stmt (List.map stmt_tree stmts)
  | Tstmt_if (cond, then_stmt, else_stmt_opt) ->
      stmt_node "IfStmt" stmt
        (expr_tree cond :: stmt_tree then_stmt
        ::
        (match else_stmt_opt with
        | None -> []
        | Some else_stmt -> [ stmt_tree else_stmt ]))
  | Tstmt_switch (cond, _, body) ->
      stmt_node "SwitchStmt" stmt [ expr_tree cond; stmt_tree body ]
  | Tstmt_while (cond, body) ->
      stmt_node "WhileStmt" stmt [ expr_tree cond; stmt_tree body ]
  | Tstmt_do (body, cond) ->
      stmt_node "DoStmt" stmt [ stmt_tree body; expr_tree cond ]
  | Tstmt_for (init_opt, cond_opt, incr_opt, body) ->
      let init_tree =
        match init_opt with None -> [] | Some init -> [ stmt_tree init ]
      in
      let cond_tree =
        match cond_opt with None -> [] | Some cond -> [ expr_tree cond ]
      in
      let incr_tree =
        match incr_opt with None -> [] | Some incr -> [ stmt_tree incr ]
      in
      stmt_node "ForStmt" stmt
        (init_tree @ cond_tree @ incr_tree @ [ stmt_tree body ])
  | Tstmt_goto target ->
      let extra fmt () = pp_identifier fmt (Some target.tlabel_name) in
      stmt_node ~extra "GotoStmt" stmt []
  | Tstmt_return None -> stmt_node "ReturnStmt" stmt []
  | Tstmt_return (Some expr) -> stmt_node "ReturnStmt" stmt [ expr_tree expr ]
  | Tstmt_break -> stmt_node "BreakStmt" stmt []
  | Tstmt_continue -> stmt_node "ContinueStmt" stmt []

and decl_tree decl =
  match decl.tdecl_kind with
  | Tdecl_value value_decl -> value_decl_tree value_decl
  | Tdecl_typedef typedef_decl -> typedef_decl_tree typedef_decl
  | Tdecl_decls [] -> node "EmptyDecls" Declaration decl.tdecl_loc []
  | Tdecl_decls [ decl ] -> decl_tree decl
  | Tdecl_decls decls ->
      node "MultipleDecls" Declaration decl.tdecl_loc (List.map decl_tree decls)

and value_decl_tree value_decl =
  match value_decl.tvalue_decl_kind with
  | Tvalue_decl_function func_decl -> function_decl_tree value_decl func_decl
  | Tvalue_decl_object object_decl -> object_decl_tree value_decl object_decl

and function_decl_tree value_decl func_decl =
  let params = List.map value_decl_tree func_decl.tfun_params in
  let body =
    match func_decl.tfun_body with
    | None -> []
    | Some stmt -> [ stmt_tree stmt ]
  in
  let children = params @ body in

  decl_node "FunctionDecl" value_decl.tvalue_decl_name
    value_decl.tvalue_decl_loc value_decl.tvalue_decl_type children

and object_decl_tree value_decl object_decl =
  ignore object_decl;
  let class_name =
    if object_decl.tvar_is_param then "ParamDecl" else "VariableDecl"
  in

  let init_tree =
    match object_decl.tvar_init with
    | None -> []
    | Some init_expr -> [ expr_tree init_expr ]
  in

  decl_node class_name value_decl.tvalue_decl_name value_decl.tvalue_decl_loc
    value_decl.tvalue_decl_type init_tree

and typedef_decl_tree typedef_decl =
  ignore typedef_decl;
  node "TypedefDecl" Declaration (Lexing.dummy_pos, Lexing.dummy_pos) []

let print_expr fmt expr =
  let tree = expr_tree expr in
  Tree_printer.print_tree print_node_info fmt tree

let print_stmt fmt stmt =
  let tree = stmt_tree stmt in
  Tree_printer.print_tree print_node_info fmt tree

let print_decl fmt decl =
  let tree = decl_tree decl in
  Tree_printer.print_tree print_node_info fmt tree

let print_translation_unit fmt tu =
  let tree =
    node "TranslationUnitDecl" Declaration
      (Lexing.dummy_pos, Lexing.dummy_pos)
      (List.map decl_tree tu)
  in
  Tree_printer.print_tree print_node_info fmt tree
