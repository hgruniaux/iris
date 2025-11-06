open Ast
open Tast

exception Error of location * string

let error loc msg = raise (Error (loc, msg))

type ctx = {
  mutable ctx_switchs : tswitch_labels list;
  ctx_symbol_table : Symbol_table.t;
  ctx_labels : (string, tlabel_declaration) Hashtbl.t;
  mutable ctx_loop_depth : int;
  ctx_struct_declarations : (string, tstruct_declaration) Hashtbl.t;
  ctx_union_declarations : (string, tstruct_declaration) Hashtbl.t;
  ctx_enum_declarations : (string, tenum_declaration) Hashtbl.t;
  mutable ctx_current_return_type : ttype option;
      (** The type of the current function's return value. *)
}

let push_scope ctx = Symbol_table.push_scope ctx.ctx_symbol_table
let pop_scope ctx = Symbol_table.pop_scope ctx.ctx_symbol_table

(** Look up [name] in the variables/functions symbol table. If [name] is not
    found, report an error. *)
let lookup_value ctx name =
  match Symbol_table.lookup_value ctx.ctx_symbol_table name.id with
  | Some v -> v
  | None ->
      let msg = Format.asprintf "Use of undeclared identifier '%s'." name.id in
      error name.loc msg

(** Look up [name] in the typedef symbol table. If [name] is not found, report
    an error. *)
let lookup_typedef ctx name =
  match Symbol_table.lookup_typedef ctx.ctx_symbol_table name.id with
  | Some t -> t
  | None ->
      let msg = Format.asprintf "Use of undeclared typedef '%s'." name.id in
      error name.loc msg

(** Get the expected return type of the current function. *)
let expected_return_type ctx = Option.get ctx.ctx_current_return_type
