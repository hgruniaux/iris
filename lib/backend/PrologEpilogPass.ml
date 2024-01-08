open Mir

(** Inserts [epilog] just before a return instruction in the given list [insts]. *)
let insert_before_ret insts epilog =
  List.concat_map
    (fun inst -> if inst.i_kind = "ret" then epilog @ [ inst ] else [ inst ])
    insts

(** Inserts the [prolog] into the given [fn] (just before the first instruction
    of the function). *)
let insert_prolog fn prolog =
  let entry_bb = Label.Map.find fn.fn_entry fn.fn_blocks in
  entry_bb.bb_insts <- prolog @ entry_bb.bb_insts

(** Inserts the [epilog] into the given [fn] (just before each return instruction). *)
let insert_epilog fn epilog =
  Label.Map.iter
    (fun _ bb -> bb.bb_insts <- insert_before_ret bb.bb_insts epilog)
    fn.fn_blocks

(** This pass inserts the prolog and epilog code of a function. *)
let pass_fn arch fn =
  let prolog = Backend.prolog arch fn in
  let epilog = Backend.epilog arch fn in
  insert_prolog fn prolog;
  insert_epilog fn epilog
