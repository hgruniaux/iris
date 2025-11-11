open Ir_base

type t = ctx

let create () =
  { mod_symbol_table = Hashtbl.create 17; mod_globals = []; mod_global_cpt = 0 }

let lookup_global m name = Hashtbl.find m.mod_symbol_table name
let lookup_global_opt m name = Hashtbl.find_opt m.mod_symbol_table name

let lookup_function (m : t) (name : string) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with
      | Iglobal_function fn -> fn
      | _ -> failwith "Global value expected to be a function")
  | None -> failwith "Global value not found"

let lookup_function_opt (m : t) (name : string) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with Iglobal_function fn -> Some fn | _ -> None)
  | None -> None

(** Returns the function named [name] in the given module [m].

    If such function does not exist, then a new function is created with the
    given [return_type] and [params_types], and added to [m].

    If the function already exists, its signature is checked (return type,
    parameters but also external qualifier).

    If [is_external] is true, the function is marked as external (defined
    elsewhere). You can not add code to an external function. *)
let get_or_create_function ?(is_external = false) (m : t) (name : string)
    (function_type : Type.t) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with
      | Iglobal_function fn ->
          assert (fn.fn_type = function_type);
          assert (fn.fn_is_external = is_external);
          global
      | _ -> failwith "Global value expected to be a function")
  | None ->
      let param_types =
        match function_type with
        | Ityp_func (params, _, _) -> params
        | _ -> assert false
      in

      let fn =
        {
          fn_name = name;
          fn_type = function_type;
          fn_params = List.mapi (fun i typ -> Reg.create i typ) param_types;
          fn_ctx = m;
          fn_symbol_table = Hashtbl.create (if is_external then 0 else 17);
          fn_blocks = LabelMap.empty;
          fn_entry = None;
          fn_is_external = is_external;
          fn_register_cpt = List.length param_types;
          fn_label_cpt = 0;
        }
      in

      let global =
        {
          global_id = Global.fresh ();
          (* FIXME: remove *)
          global_name = Some name;
          global_mutable = false;
          global_kind = Iglobal_function fn;
        }
      in

      Hashtbl.add m.mod_symbol_table name global;
      m.mod_globals <- global :: m.mod_globals;
      global

let lookup_variable (m : t) (name : string) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with
      | Iglobal_variable (var_type, _) -> (var_type, global)
      | _ -> failwith "Global value expected to be a variable")
  | None -> failwith "Global value not found"

let lookup_variable_opt (m : t) (name : string) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with
      | Iglobal_variable (var_type, _) -> Some (var_type, global)
      | _ -> None)
  | None -> None

(** Returns the variable named [name] in the given module [m].

    If such variable does not exist, then a new variable is created with the
    given [typ], and added to [m].

    If [is_mutable] is true, the variable is marked as mutable. If false, it is
    marked as immutable.

    If the variable already exists, its type is checked. *)
let get_or_create_variable ?(is_mutable = true) (m : t) (name : string)
    (typ : Type.t) =
  match lookup_global_opt m name with
  | Some global -> (
      match global.global_kind with
      | Iglobal_variable (var_type, _) ->
          assert (var_type = typ);
          global
      | _ -> failwith "Global value expected to be a variable")
  | None ->
      let global =
        {
          global_id = Global.fresh ();
          (* FIXME: remove *)
          global_name = Some name;
          global_mutable = is_mutable;
          global_kind = Iglobal_variable (typ, Iconstant_uninitialized);
        }
      in
      Hashtbl.add m.mod_symbol_table name global;
      m.mod_globals <- global :: m.mod_globals;
      global

(** Creates an unnamed string constant in the given module [m]. The type of the
    created global is an array of 8-bit integers. No null-terminator is added to
    [value], you may need to add one manually if required. *)
let create_string_constant m value =
  let bytes = Bytes.of_string value in
  let id = m.mod_global_cpt in
  m.mod_global_cpt <- m.mod_global_cpt + 1;

  let global =
    {
      global_id = Global.fresh_with_id id;
      global_name = None;
      global_mutable = false;
      global_kind =
        Iglobal_variable
          (Ityp_array (Ityp_i8, Bytes.length bytes), Iconstant_bytes bytes);
    }
  in

  m.mod_globals <- global :: m.mod_globals;
  global

let iter_globals f (m : t) =
  Hashtbl.iter (fun _ global -> f global) m.mod_symbol_table

let iter_functions f (m : t) =
  Hashtbl.iter
    (fun _ global ->
      match global.global_kind with Iglobal_function fn -> f fn | _ -> ())
    m.mod_symbol_table

let fold_functions f (m : t) init =
  Hashtbl.fold
    (fun _ global acc ->
      match global.global_kind with Iglobal_function fn -> f fn acc | _ -> acc)
    m.mod_symbol_table init
