(*
  A set of utilities functions to implement MIR passes.
*)

open Mir

(** Calls [f] on each instruction of the MIR function [fn]. *)
let iter_insts fn f =
  Label.Map.iter (fun _ bb -> List.iter f bb.bb_insts) fn.fn_blocks

(* Transforms each instruction of the MIR function [fn] using [f]. *)
let map_insts fn f =
  Label.Map.iter
    (fun _ bb ->
      bb.bb_insts <-
        List.fold_right
          (fun inst acc ->
            let new_insts = f inst in
            new_insts @ acc)
          bb.bb_insts [])
    fn.fn_blocks

(** Inserts the sequence of instruction [prolog] at the start of the MIR function [fn]. *)
let insert_prolog fn prolog =
  let entry_bb = Label.Map.find fn.fn_entry fn.fn_blocks in
  entry_bb.bb_insts <- prolog @ entry_bb.bb_insts

(** Inserts the sequence of instruction [epilog] just before each return instruction in
    the given MIR function [fn]. *)
let insert_epilog fn epilog =
  let insert_before_ret insts epilog =
    (* FIXME: more portable way to detect a return instruction *)
    List.concat_map
      (fun inst -> if inst.i_kind = "ret" then epilog @ [ inst ] else [ inst ])
      insts
  in

  Label.Map.iter
    (fun _ bb -> bb.bb_insts <- insert_before_ret bb.bb_insts epilog)
    fn.fn_blocks
