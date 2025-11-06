open Tast
open Typing_common
open Typing_cast

let create_param_decl typ name loc =
  let var_decl =
    {
      tvar_name = name;
      tvar_type = typ;
      tvar_storage = Tstorage_auto;
      tvar_is_param = true;
      tvar_init = None;
      tvar_loc = loc;
    }
  in
  let value_decl =
    {
      tvalue_decl_name = name;
      tvalue_decl_kind = Tvalue_decl_object var_decl;
      (* C23 6.2.2 §6
         The following identifiers have no linkage: (...); an identifier
         declared to be a function parameter; (...). *)
      tvalue_decl_linkage = Tlinkage_none;
      tvalue_decl_type = typ;
      tvalue_decl_loc = loc;
    }
  in
  value_decl

let check_previous_value_declaration ctx (name : identifier) var_typ =
  match Symbol_table.lookup_value ctx.ctx_symbol_table name.id with
  | None -> (var_typ, None)
  | Some previous_decl ->
      (* C23 6.7.1 §4
         If an identifier has no linkage, there shall be no more than one
         declaration of the identifier (in a declarator or type specifier)
         with the same scope and in the same name space (...). *)
      (* TODO: Handle linkages correctly *)

      (* C23 6.7.1 §5
         All declarations in the same scope that refer to the same object
         or function shall specify compatible types. *)
      let previous_type = previous_decl.tvalue_decl_type in
      if not (are_compatible var_typ previous_type) then
        let msg =
          Printf.sprintf "Redeclaration of '%s' with incompatible type." name.id
        in
        error name.loc msg
      else
        let new_type = composite_type_from previous_type var_typ in
        (new_type, Some previous_decl)

let handle_function_declaration ctx loc typ name params =
  let return_type, _, _ = Type.destruct_function typ in

  let func_decl =
    {
      tfun_name = name;
      tfun_type = typ;
      tfun_params = params;
      tfun_return_type = return_type;
      tfun_body = None;
    }
  in

  let value_decl =
    {
      tvalue_decl_name = Some name;
      tvalue_decl_kind = Tvalue_decl_function func_decl;
      tvalue_decl_linkage = Tlinkage_external;
      tvalue_decl_type = typ;
      tvalue_decl_loc = loc;
    }
  in

  Symbol_table.replace_value ctx.ctx_symbol_table name.id value_decl;
  value_decl

let check_parameters_are_complete (params : tvalue_decl list) =
  (* C23 6.7.7.4 §3
     After adjustment, the parameters in a parameter type list in a function
     declarator that is part of a definition of that function shall not have
     incomplete type. *)
  List.iter
    (fun param ->
      if Type.is_incomplete param.tvalue_decl_type then
        let name, loc =
          match param.tvalue_decl_name with
          | Some name -> (Format.asprintf "'%s' " name.id, name.loc)
          | None -> ("", param.tvalue_decl_loc)
        in
        let msg =
          Format.asprintf
            "Function parameter %scannot have incomplete type in function \
             definition."
            name
        in
        error loc msg)
    params

let handle_function_definition ctx loc typ (name : identifier) params =
  let typ, _ = check_previous_value_declaration ctx name typ in
  let return_type, _, _ = Type.destruct_function typ in

  (* C23 6.7.7.4 §3
     After adjustment, the parameters in a parameter type list in a function
     declarator that is part of a definition of that function shall not have
     incomplete type. *)
  check_parameters_are_complete params;

  let func_decl =
    {
      tfun_name = name;
      tfun_type = typ;
      tfun_params = params;
      tfun_return_type = return_type;
      tfun_body = None;
    }
  in

  let value_decl =
    {
      tvalue_decl_name = Some name;
      tvalue_decl_kind = Tvalue_decl_function func_decl;
      tvalue_decl_linkage = Tlinkage_external;
      tvalue_decl_type = typ;
      tvalue_decl_loc = loc;
    }
  in

  Symbol_table.replace_value ctx.ctx_symbol_table name.id value_decl;
  (value_decl, func_decl)

let handle_variable_declaration ctx loc typ name =
  let linkage =
    if Symbol_table.in_global_scope ctx.ctx_symbol_table then Tlinkage_external
    else Tlinkage_none
  in

  let var_decl =
    {
      tvar_name = Some name;
      tvar_type = typ;
      tvar_storage = Tstorage_auto;
      tvar_is_param = false;
      tvar_init = None;
      tvar_loc = loc;
    }
  in

  let value_decl =
    {
      tvalue_decl_name = Some name;
      tvalue_decl_kind = Tvalue_decl_object var_decl;
      tvalue_decl_linkage = linkage;
      tvalue_decl_type = typ;
      tvalue_decl_loc = loc;
    }
  in

  Symbol_table.replace_value ctx.ctx_symbol_table name.id value_decl;
  value_decl

let handle_variable_definition ctx loc typ (name : identifier) init_opt
    previous_decl_opt =
  (if Option.is_some previous_decl_opt then
     let msg = Printf.sprintf "Redefinition of variable '%s'." name.id in
     error name.loc msg);

  let linkage =
    if Symbol_table.in_global_scope ctx.ctx_symbol_table then Tlinkage_external
    else Tlinkage_none
  in

  let var_decl =
    {
      tvar_name = Some name;
      tvar_type = typ;
      tvar_storage = Tstorage_auto;
      tvar_is_param = false;
      tvar_init = init_opt;
      tvar_loc = loc;
    }
  in

  let value_decl =
    {
      tvalue_decl_name = Some name;
      tvalue_decl_kind = Tvalue_decl_object var_decl;
      tvalue_decl_linkage = linkage;
      tvalue_decl_type = typ;
      tvalue_decl_loc = loc;
    }
  in

  Symbol_table.replace_value ctx.ctx_symbol_table name.id value_decl;
  value_decl

(** Handles the declaration of a variable or function. *)
let handle_value_declaration ctx loc typ (name : identifier) params init_opt =
  let is_function = Type.is_function typ in

  (* TODO: Handle correctly linkage
           For now, assume all identifiers in global scope have external linkage,
           so we can have multiple definition tentative on them. But local variables,
           have internal linkage and so their declaration is also a definition. *)
  let is_external = Symbol_table.in_global_scope ctx.ctx_symbol_table in

  let new_type, previous_decl_opt =
    check_previous_value_declaration ctx name typ
  in

  match init_opt with
  | Some init when is_function ->
      let msg = "Function declarations cannot have initializers." in
      error init.texpr_loc msg
  | Some init ->
      let init_converted = try_implicit_cast_to new_type init in
      handle_variable_definition ctx loc new_type name (Some init_converted)
        previous_decl_opt
  | None ->
      if is_function then
        handle_function_declaration ctx loc new_type name params
      else if is_external then handle_variable_declaration ctx loc new_type name
      else
        handle_variable_definition ctx loc new_type name None previous_decl_opt
