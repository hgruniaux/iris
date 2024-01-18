open Ir

type t = term

let successors_of = function
  | Iterm_unreachable | Iterm_ret | Iterm_retv _ -> Label.Set.empty
  | Iterm_jmp bb -> Label.Set.singleton bb
  | Iterm_jmpc (_, then_bb, else_bb) -> Label.Set.of_list [ then_bb; else_bb ]
