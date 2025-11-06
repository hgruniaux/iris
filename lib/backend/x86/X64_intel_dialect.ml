open Format
open X64_inst

let print_reg fmt r =
  match r.reg_physical with
  | Some pr -> fprintf fmt "%s" (X64_reg_names.physical_reg_name pr)
  | None -> fprintf fmt "%%%d" r.reg_id

let print_imm fmt i = Z.pp_print fmt i

let print_mem fmt mem =
  check_x64_mem mem;

  let print_disp fmt d =
    let print_int fmt d =
      if d < 10 then fprintf fmt "%d" d else fprintf fmt "%#x" d
    in

    if d == 0 then ()
    else if d > 0 then fprintf fmt " + %a" print_int d
    else fprintf fmt " - %a" print_int (-d)
  in

  let print_scale fmt scale =
    if scale == 1 then () else fprintf fmt "%d * " scale
  in

  match (mem.base, mem.index, mem.scale, mem.displacement) with
  | None, None, _, d -> fprintf fmt "[%d]" d
  | Some base, None, scale, d ->
      fprintf fmt "[%a%a%a]" print_scale scale print_reg base print_disp d
  | None, Some _, _, _ -> assert false (* impossible *)
  | Some base, Some index, scale, d ->
      fprintf fmt "[%a + %a%a%a]" print_reg base print_scale scale print_reg
        index print_disp d

let print_reg_mem fmt rom =
  match rom with
  | X64_rom_reg r -> print_reg fmt r
  | X64_rom_mem m -> print_mem fmt m

let print_reg_mem_imm fmt rmi =
  match rmi with
  | X64_rmi_reg r -> print_reg fmt r
  | X64_rmi_mem m -> print_mem fmt m
  | X64_rmi_imm i -> print_imm fmt i

let print_label fmt label = Label.pp_print fmt label

let print_cc fmt cc =
  match cc with X64_cc_e -> fprintf fmt "e" | X64_cc_ne -> fprintf fmt "ne"

let print_inst fmt inst =
  match inst with
  | X64_inst_nop -> fprintf fmt "nop"
  | X64_inst_mov (dst, src) ->
      fprintf fmt "mov %a, %a" print_reg_mem dst print_reg_mem_imm src
  | X64_inst_lea (dst, mem) ->
      fprintf fmt "lea %a, %a" print_reg dst print_mem mem
  | X64_inst_add (dst, src) ->
      fprintf fmt "add %a, %a" print_reg_mem dst print_reg_mem_imm src
  | X64_inst_sub (dst, src) ->
      fprintf fmt "sub %a, %a" print_reg_mem dst print_reg_mem_imm src
  | X64_inst_jmp label -> fprintf fmt "jmp %a" print_label label
  | X64_inst_jmpcc (cc, true_label, _) ->
      fprintf fmt "j%a %a" print_cc cc print_label true_label
      (* We do know print [false_label] as the real instruction only takes the
         true label. The false label will be emitted either as a separate unconditional
         jump or as the source code of the basic block. This is handled by the full
         printer and not by the dialect. *)
  | X64_inst_ret -> fprintf fmt "ret"
  | X64_inst_ud2 -> fprintf fmt "ud2"
