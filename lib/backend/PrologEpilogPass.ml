open Mir

(** This pass inserts the prolog and epilog code of a function. Generally,
    these two allocate and free the function's stack frame. *)
let pass_fn arch fn =
  let prolog = Backend.prolog arch fn in
  let epilog = Backend.epilog arch fn in
  MirPassUtils.insert_prolog fn prolog;
  MirPassUtils.insert_epilog fn epilog
