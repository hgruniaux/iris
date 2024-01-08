open Mir

type emit_ctx = {
  cur_fn : Mir.fn;
  mutable already_emitted_bbs : Label.Set.t;
}

let mk_ctx cur_fn =
  { cur_fn; already_emitted_bbs = Label.Set.empty }

let resolve_label ctx l = Label.Map.find l ctx.cur_fn.fn_blocks
let is_bb_already_emitted ctx l = Label.Set.mem l ctx.already_emitted_bbs
let emit_fn_header ppf fname = Format.fprintf ppf "%s:\n" fname

let reg_name r =
  if r = CPUlmRegs.rout then "rout"
  else if r = CPUlmRegs.rsp then "rsp"
  else if r = CPUlmRegs.rpriv then "rpriv"
  else Format.asprintf "r%d" r

let emit_label = IrPrettyPrinter.pp_label

let emit_operand ppf operand =
  match operand with
  | Oreg r -> Format.fprintf ppf "%s" (reg_name r)
  | Oframe n -> Format.fprintf ppf "STACK[%d]" n
  | Oimm i -> Format.fprintf ppf "%s" (Nativeint.to_string i)
  | Olabel l -> emit_label ppf l
  | Ofunc f -> Format.fprintf ppf "%s" f.fn_name

let rec emit_operand_list ctx ppf operands =
  match operands with
  | [] -> ()
  | o :: r ->
      Format.fprintf ppf "%a %a" (emit_operand) o (emit_operand_list ctx) r

module SSet = Set.Make (String)

let jmpc_insts = SSet.of_list [ "jmp.z"; "jmp.n"; "jmp.c"; "jmp.v" ]

let rec emit_inst ctx ppf inst =
  match inst.i_kind with
  | "mov" -> (
      match inst.i_operands with
      (* Do not emit a mov between the same registers. *)
      | [ Oreg r1; Oreg r2 ] when r1 = r2 -> ()
      | _ ->
          Format.fprintf ppf "  mov %a\n" (emit_operand_list ctx)
            inst.i_operands)
  | "jmp" -> emit_jmp_inst ctx ppf inst
  | opname when SSet.mem opname jmpc_insts -> emit_jmpc_inst ctx ppf inst
  | opname ->
      Format.fprintf ppf "  %s %a\n" opname (emit_operand_list ctx)
        inst.i_operands

and emit_bb_or_jmp ctx ppf l =
  if is_bb_already_emitted ctx l then
    Format.fprintf ppf "  jmp %a\n" emit_label l
  else
    let target_bb = resolve_label ctx l in
    emit_bb ctx ppf target_bb

and emit_jmp_inst ctx ppf inst =
  let l =
    match inst.i_operands with
    | [ Olabel l ] -> l
    | _ -> failwith "invalid operands to jmp instruction on CPUlm"
  in
  emit_bb_or_jmp ctx ppf l

and emit_jmpc_inst ctx ppf inst =
  let then_label, else_label =
    match inst.i_operands with
    | [ Olabel tl; Olabel el ] -> (tl, el)
    | _ -> failwith "invalid operands to conditional jmp instruction on CPUlm"
  in
  Format.fprintf ppf "  %s %a\n" inst.i_kind emit_label then_label;
  emit_bb_or_jmp ctx ppf else_label;
  emit_bb ctx ppf (resolve_label ctx then_label)

and emit_insts ctx ppf insts =
  match insts with
  | [] -> ()
  | inst :: r ->
      emit_inst ctx ppf inst;
      emit_insts ctx ppf r

and emit_bb ctx ppf bb =
  if is_bb_already_emitted ctx bb.bb_label then ()
  else (
    ctx.already_emitted_bbs <- Label.Set.add bb.bb_label ctx.already_emitted_bbs;
    Format.fprintf ppf "%a:\n%a" emit_label bb.bb_label (emit_insts ctx)
      bb.bb_insts)

let emit_fn ppf fn =
  emit_fn_header ppf fn.fn_name;
  let ctx = mk_ctx fn in
  let entry_bb = Label.Map.find fn.fn_entry fn.fn_blocks in
  emit_bb ctx ppf entry_bb
