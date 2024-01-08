open Ir

(** This pass removes dead code instructions. That is, all instructions that
    have no side effects and that are not used in the program are removed. *)
let pass_fn fn =
  let changed = ref true in
  while !changed do
    let marked_insts = ref [] in

    Label.Map.iter
      (fun _ bb ->
        iter_insts
          (fun inst ->
            if inst.i_uses = [] && not (has_sideeffect inst) then
              marked_insts := inst :: !marked_insts)
          bb)
      fn.fn_blocks;

    changed := !marked_insts <> [];
    List.iter (fun inst -> remove inst) !marked_insts
  done
