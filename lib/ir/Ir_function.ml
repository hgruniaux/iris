open Ir_base

type t = fn

(** Creates a new label for the given function [f]. *)
let fresh_label ?(name = None) (f : fn) : Label.t =
  let id = f.fn_label_cpt in
  f.fn_label_cpt <- f.fn_label_cpt + 1;
  Label.create ~name id

(** Creates a new register for the given function [f]. *)
let fresh_register ?(name = None) (f : fn) (typ : Type.t) : Reg.t =
  let id = f.fn_register_cpt in
  f.fn_register_cpt <- f.fn_register_cpt + 1;
  Reg.create ~name id typ

(** Returns the return type of the given function [f]. *)
let return_type_of (f : fn) : Type.t =
  match f.fn_type with
  | Ityp_func (_, ret_typ, _) -> ret_typ
  | _ -> assert false

(** Returns the parameter types of the given function [f]. *)
let params_types_of (f : fn) : Type.t list =
  match f.fn_type with
  | Ityp_func (param_tys, _, _) -> param_tys
  | _ -> assert false

(** Returns the return, parameter types and variadic status of the given
    function [f]. *)
let return_and_params_types_of (f : fn) : Type.t * Type.t list * bool =
  match f.fn_type with
  | Ityp_func (param_tys, ret_typ, is_variadic) ->
      (ret_typ, param_tys, is_variadic)
  | _ -> assert false

let entry_block (f : fn) : bb option =
  match f.fn_entry with
  | None -> None
  | Some entry_label -> Some (LabelMap.find entry_label f.fn_blocks)

let iter_blocks (fnc : bb -> unit) (f : fn) : unit =
  LabelMap.iter (fun _ bb -> fnc bb) f.fn_blocks

let fold_blocks (fnc : bb -> 'a -> 'a) (f : fn) (acc : 'a) : 'a =
  LabelMap.fold (fun _ bb acc -> fnc bb acc) f.fn_blocks acc
