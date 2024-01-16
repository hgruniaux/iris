(*
  A set of utilities functions to implement Mr passes.
*)

open Mr

(** Calls [f] on each instruction of the Mr function [fn]. *)
let iter_insts fn f =
  Label.Map.iter (fun _ bb -> List.iter f bb.mbb_insts) fn.mfn_blocks

(* Transforms each instruction of the Mr function [fn] using [f]. *)
let map_insts fn f =
  Label.Map.iter
    (fun _ bb ->
      bb.mbb_insts <-
        List.fold_right
          (fun inst acc ->
            let new_insts = f inst in
            new_insts @ acc)
          bb.mbb_insts [])
    fn.mfn_blocks

(** Inserts the sequence of instruction [prolog] at the start of the Mr function [fn]. *)
let insert_prolog fn prolog =
  let entry_bb = Label.Map.find fn.mfn_entry fn.mfn_blocks in
  entry_bb.mbb_insts <- prolog @ entry_bb.mbb_insts

(** Inserts the sequence of instruction [epilog] just before each return instruction in
    the given Mr function [fn]. *)
let insert_epilog fn epilog =
  let insert_before_ret insts epilog =
    (* FIXME: more portable way to detect a return instruction *)
    List.concat_map
      (fun inst -> if inst.mi_kind = "ret" then epilog @ [ inst ] else [ inst ])
      insts
  in

  Label.Map.iter
    (fun _ bb -> bb.mbb_insts <- insert_before_ret bb.mbb_insts epilog)
    fn.mfn_blocks
