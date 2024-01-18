open Mr

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

(** Same as List.map2 but stop when l1 is empty (therefore both lists may
        have a different length). *)
let rec map2 f l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> failwith "not enough elements in l2"
  | x1 :: r1, x2 :: r2 -> f x1 x2 :: map2 f r1 r2

(** This pass implements the loading mechanism for parameters. It inserts
    either move instructions from the physical register to the pseudo registers
    representing parameters or it inserts frame object load instructions
    to retrieve parameters from the stack (depending on the calling convention). *)
let pass_fn arch fn =
  let cc_info = Backend.cc_info arch in

  let args_in_regs, args_in_stack =
    firstk cc_info.cc_args_regs_count fn.mfn_params
  in

  (* Inserts the move instructions for the parameters lying in registers. *)
  let mov_insts =
    map2 (fun arg reg -> Mr.mk_mov arg reg) args_in_regs cc_info.cc_args_regs
  in

  (* Inserts the frame load instructions for the parameters lying in the stack. *)
  (* We start the frame index at -2, because -1 is used for the return address and
     0 for the saved EBP. *)
  (* FIXME: more portable, the constant -2 is specific to x86 *)
  let cur_frame_idx = ref (-2) in
  let load_insts =
    List.fold_right
      (fun reg insts ->
        let result = Mr.mk_stack_load reg !cur_frame_idx :: insts in
        decr cur_frame_idx;
        result)
      args_in_stack []
  in

  (* Reverse push instructions if arguments are expected left to right. *)
  let load_insts =
    if cc_info.cc_args_stack_ltr then List.rev load_insts else load_insts
  in

  (* Insert the code at start of the function. *)
  MirPassUtils.insert_prolog fn (mov_insts @ load_insts)