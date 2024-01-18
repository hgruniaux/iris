open Ir

type t = term

(** Returns the successors of a given terminator function. *)
let successors_of = function
  | Iterm_unreachable | Iterm_ret | Iterm_retv _ -> Label.Set.empty
  | Iterm_jmp bb -> Label.Set.singleton bb
  | Iterm_jmpc (_, then_bb, else_bb) -> Label.Set.of_list [ then_bb; else_bb ]
  | Iterm_switch (_, otherwise, targets) ->
      Label.Set.add otherwise
        (List.fold_left
           (fun acc (_, label) -> Label.Set.add label acc)
           Label.Set.empty targets)
