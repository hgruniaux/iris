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

(** Returns the first [k] elements of [xs] and the remaining elements.
    If [xs] is too small, then [xs] is returned. *)
let rec firstk k xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
      if k = 0 then ([], x :: xs)
      else if k = 1 then ([ x ], xs)
      else
        let f, r = firstk (k - 1) xs in
        (x :: f, r)

(** Same as List.iter2 except that the function stop, normally, when [l1] has
    no more elements. However, it is an error if [l2] has not enough elements. *)
let rec lax_iter2 f l1 l2 =
  match (l1, l2) with
  | [], _ -> ()
  | _, [] -> failwith "lax_iter2: not enough elements in l2"
  | x1 :: tl1, x2 :: tl2 ->
      f x1 x2;
      lax_iter2 f tl1 tl2

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
