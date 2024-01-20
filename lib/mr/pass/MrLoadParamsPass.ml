open Mr
open MrPassUtils

module type MrBuilder = sig
  val mk_mov_reg : Mr.reg -> Mr.reg -> Mr.minst list
  val mk_frame_load : Mr.reg -> int -> Mr.minst list
end

module Make (Builder : MrBuilder) = struct
  (** This pass implements the loading mechanism for parameters. It inserts
      either move instructions from the physical register to the pseudo registers
      representing parameters or it inserts frame object load instructions
      to retrieve parameters from the stack (depending on the calling convention). *)
  let pass_fn _ fn =
    let cc_info = fn.mfn_cc_info in

    let args_in_regs, args_in_stack =
      firstk cc_info.cc_args_regs_count fn.mfn_params
    in

    (* Inserts the move instructions for the parameters lying in registers. *)
    let mov_insts =
      let insts = ref [] in
      lax_iter2
        (fun arg reg -> insts := Builder.mk_mov_reg arg reg @ !insts)
        args_in_regs cc_info.cc_args_regs;
      !insts
    in

    (* Inserts the frame load instructions for the parameters lying in the stack. *)
    (* We start the frame index at -2, because -1 is used for the return address and
       0 for the saved EBP. *)
    (* FIXME: more portable, the constant -2 is specific to x86 *)
    let cur_frame_idx = ref (-2) in
    let load_insts =
      List.fold_right
        (fun reg insts ->
          let result = Builder.mk_frame_load reg !cur_frame_idx @ insts in
          decr cur_frame_idx;
          result)
        args_in_stack []
    in

    (* Insert the code at start of the function. *)
    MrPassUtils.insert_prolog fn (mov_insts @ load_insts)
end
