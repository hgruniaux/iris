(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

type 'a tree = Node of 'a * 'a tree list

(** [print_tree print_val tree] prints the given [tree] to the standard OCaml
    formatter. [print_val] is a function that prints a node's value of type
    ['a]. *)
let print_tree print_val fmt (tree : 'a tree) =
  let rec print_subtree prefix is_last (Node (value, children)) =
    let connector = if is_last then "`-" else "|-" in
    Format.fprintf fmt "%s%s%a@." prefix connector print_val value;

    let child_prefix = prefix ^ if is_last then "  " else "| " in

    let rec iter_children children =
      match children with
      | [] -> () (* Base case: no more children *)
      | [ last_child ] ->
          (* Last child case: print with is_last = true *)
          print_subtree child_prefix true last_child
      | head_child :: tail_children ->
          (* Not the last child: print with is_last = false *)
          print_subtree child_prefix false head_child;
          (* Recurse on the rest of the children *)
          iter_children tail_children
    in

    iter_children children
  in

  match tree with
  | Node (value, children) ->
      print_val fmt value;
      Format.fprintf fmt "@.";

      let rec iter_children children =
        match children with
        | [] -> ()
        | [ last_child ] -> print_subtree "" true last_child
        | head_child :: tail_children ->
            print_subtree "" false head_child;
            iter_children tail_children
      in

      iter_children children;

      Format.fprintf fmt "@."
