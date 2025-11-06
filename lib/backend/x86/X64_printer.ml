open X64_inst
open Format

module type Dialect = sig
  val print_inst : Format.formatter -> x64_inst -> unit
end

module Printer (D : Dialect) = struct
  type fun_ctx = {
    current_function : x64_function;
    mutable emitted_labels : LabelSet.t;
  }

  (** Prints the instruction [fmt] using the printer's dialect with indentation
      and newline. This function do not handle properly the case of jump
      instructions. *)
  let print_inst_raw fmt inst =
    fprintf fmt "  ";
    D.print_inst fmt inst;
    fprintf fmt "\n"

  (** Either prints basic block corresponding to [label] or a jump instruction
      to [label], depending on if [label]'s basic block has already been
      emitted. *)
  let rec print_jmp_or_block ctx fmt label =
    (* We have two possibilities:
       - either [label]'s basic block was already emitted, and therefore
         we need to emit a jump to it,
       - either [label]'s basic block is not yet emitted, in which case
         we can emit the full basic block now and avoid a jmp instruction. *)
    if LabelSet.mem label ctx.emitted_labels then
      print_inst_raw fmt (X64_inst_jmp label)
    else
      let target_block =
        LabelMap.find label ctx.current_function.x64_fn_blocks
      in
      print_basic_block ctx fmt target_block

  (** Prints all instructions in a basic block. *)
  and print_instructions ctx fmt insts =
    List.iter
      (function
        | X64_inst_mov (X64_rom_reg dst, X64_rmi_reg src) when dst = src -> ()
        | X64_inst_jmp label -> print_jmp_or_block ctx fmt label
        | X64_inst_jmpcc (_, _, false_label) as inst ->
            print_inst_raw fmt inst;
            print_jmp_or_block ctx fmt false_label
        | inst -> print_inst_raw fmt inst)
      insts

  (** Prints a basic block instructions and its label. *)
  and print_basic_block ctx fmt bb =
    assert (not (LabelSet.mem bb.x64_bb_label ctx.emitted_labels));
    ctx.emitted_labels <- LabelSet.add bb.x64_bb_label ctx.emitted_labels;

    Format.fprintf fmt "%a:\n" Label.pp_print bb.x64_bb_label;
    print_instructions ctx fmt bb.x64_bb_insts

  (** Prints a function definition. *)
  and print_function fmt fn =
    Format.fprintf fmt ".global %s\n" fn.x64_fn_name;
    Format.fprintf fmt ".type %s, @function\n" fn.x64_fn_name;
    Format.fprintf fmt "%s:\n" fn.x64_fn_name;
    let entry_block = LabelMap.find fn.x64_fn_entry fn.x64_fn_blocks in
    print_basic_block
      { current_function = fn; emitted_labels = LabelSet.empty }
      fmt entry_block
end

module IntelPrinter = Printer (X64_intel_dialect)
