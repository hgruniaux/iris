open Ir_base
module Value = Ir_value
module ValueSet = Set.Make (Value)

type t = terminator

(** Returns the set of values used by a given terminator instruction. *)
let uses = function
  | Iterm_unreachable | Iterm_ret None -> ValueSet.empty
  | Iterm_ret (Some value) -> ValueSet.singleton value
  | Iterm_br (_, args) -> ValueSet.of_list args
  | Iterm_br_if (cond, _, true_args, _, false_args) ->
      ValueSet.add cond
        (ValueSet.union
           (ValueSet.of_list true_args)
           (ValueSet.of_list false_args))
  | Iterm_br_table (cond, _, default_args, cases) ->
      List.fold_left
        (fun acc (_, _, case_args) ->
          ValueSet.union acc (ValueSet.of_list case_args))
        (ValueSet.add cond (ValueSet.of_list default_args))
        cases

(** Returns the set of registers used by a given terminator instruction. *)
let uses_reg term =
  let values = uses term in
  ValueSet.fold
    (fun v acc -> match v with Ival_reg r -> RegSet.add r acc | _ -> acc)
    values RegSet.empty

(** Returns the set of values defined by a given terminator instruction. *)
let defs _ = ValueSet.empty

(** Returns the set of registers defined by a given terminator instruction. *)
let defs_reg _ = RegSet.empty

(** Maps the values used by a given terminator instruction. *)
let map_values f term =
  match term with
  | Iterm_unreachable -> term
  | Iterm_ret None -> term
  | Iterm_ret (Some value) -> Iterm_ret (Some (f value))
  | Iterm_br _ -> term
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      Iterm_br_if
        ( f cond,
          true_label,
          List.map f true_args,
          false_label,
          List.map f false_args )
  | Iterm_br_table (cond, default_label, default_args, cases) ->
      Iterm_br_table
        ( f cond,
          default_label,
          List.map f default_args,
          List.map (fun (i, label, args) -> (i, label, List.map f args)) cases
        )

(** Maps the registers used by a given terminator instruction. *)
let map_regs f term = map_values (fun v -> Value.map_reg f v) term

(** Maps the labels used by a given terminator instruction. *)
let map_labels f term =
  match term with
  | Iterm_unreachable -> term
  | Iterm_ret _ -> term
  | Iterm_br (target_label, args) -> Iterm_br (f target_label, args)
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      Iterm_br_if (cond, f true_label, true_args, f false_label, false_args)
  | Iterm_br_table (cond, default_label, default_args, cases) ->
      Iterm_br_table
        ( cond,
          f default_label,
          default_args,
          List.map (fun (i, label, args) -> (i, f label, args)) cases )

(** Maps the values and labels used by a given terminator instruction. This is
    equivalent to [map_values f_val (map_labels f_label term)]. *)
let map_values_and_labels f_val f_label term =
  match term with
  | Iterm_unreachable -> term
  | Iterm_ret None -> term
  | Iterm_ret (Some value) -> Iterm_ret (Some (f_val value))
  | Iterm_br (target_label, args) ->
      Iterm_br (f_label target_label, List.map f_val args)
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      Iterm_br_if
        ( f_val cond,
          f_label true_label,
          List.map f_val true_args,
          f_label false_label,
          List.map f_val false_args )
  | Iterm_br_table (cond, default_label, default_args, cases) ->
      Iterm_br_table
        ( f_val cond,
          f_label default_label,
          List.map f_val default_args,
          List.map
            (fun (i, label, args) -> (i, f_label label, List.map f_val args))
            cases )

(** Returns the successors of a given terminator instruction. *)
let succs = function
  | Iterm_unreachable | Iterm_ret _ -> LabelSet.empty
  | Iterm_br (target_label, _) -> LabelSet.singleton target_label
  | Iterm_br_if (_, true_label, _, false_label, _) ->
      LabelSet.of_list [ true_label; false_label ]
  | Iterm_br_table (_, default_label, _, cases) ->
      LabelSet.add default_label
        (List.fold_left
           (fun acc (_, label, _) -> LabelSet.add label acc)
           LabelSet.empty cases)

(** Checks if it is the return instruction. *)
let is_ret = function Iterm_ret _ -> true | _ -> false

(** Checks if it is the unreachable instruction. *)
let is_unreachable = function Iterm_unreachable -> true | _ -> false
