open Mir

type emit_ctx = {
  is_x64 : bool;
  cur_fn : Mir.fn;
  mutable already_emitted_bbs : Label.Set.t;
}

let mk_ctx is_x64 cur_fn =
  { is_x64; cur_fn; already_emitted_bbs = Label.Set.empty }

let resolve_label ctx l = Label.Map.find l ctx.cur_fn.fn_blocks
let is_bb_already_emitted ctx l = Label.Set.mem l ctx.already_emitted_bbs
let emit_preamble ppf = Format.fprintf ppf ".intel_syntax noprefix\n\n"

let emit_fn_header ppf fname =
  Format.fprintf ppf ".globl %s\n.type %s, @@function\n%s:\n" fname fname fname

let x86_reg_names =
  List.fold_left
    (fun m (r, name) -> Reg.Map.add r name m)
    Reg.Map.empty
    [
      (X86Regs.eax, "eax");
      (X86Regs.ebx, "ebx");
      (X86Regs.ecx, "ecx");
      (X86Regs.edx, "edx");
      (X86Regs.esp, "esp");
      (X86Regs.ebp, "ebp");
      (X86Regs.esi, "esi");
      (X86Regs.edi, "edi");
    ]

let x64_reg_names =
  List.fold_left
    (fun m (r, name) -> Reg.Map.add r name m)
    Reg.Map.empty
    [
      (X86Regs.rax, "rax");
      (X86Regs.rbx, "rbx");
      (X86Regs.rcx, "rcx");
      (X86Regs.rdx, "rdx");
      (X86Regs.rsp, "rsp");
      (X86Regs.rbp, "rbp");
      (X86Regs.rsi, "rsi");
      (X86Regs.rdi, "rdi");
      (X86Regs.r8, "r8");
      (X86Regs.r9, "r9");
      (X86Regs.r10, "r10");
      (X86Regs.r11, "r11");
      (X86Regs.r12, "r12");
      (X86Regs.r13, "r13");
      (X86Regs.r14, "r14");
      (X86Regs.r15, "r15");
    ]

let name_of_reg ctx r =
  if ctx.is_x64 then Reg.Map.find r x64_reg_names
  else Reg.Map.find r x86_reg_names

let byte_reg_names =
  List.fold_left
    (fun m (r, name) -> Reg.Map.add r name m)
    Reg.Map.empty
    [
      (X86Regs.rax, "al");
      (X86Regs.rbx, "bl");
      (X86Regs.rcx, "cl");
      (X86Regs.rdx, "dl");
      (X86Regs.rsp, "spl");
      (X86Regs.rbp, "bpl");
      (X86Regs.rsi, "sil");
      (X86Regs.rdi, "dil");
      (X86Regs.r8, "r8b");
      (X86Regs.r9, "r9b");
      (X86Regs.r10, "r10b");
      (X86Regs.r11, "r11b");
      (X86Regs.r12, "r12b");
      (X86Regs.r13, "r13b");
      (X86Regs.r14, "r14b");
      (X86Regs.r15, "r15b");
    ]

let name_of_byte_reg r = Reg.Map.find r byte_reg_names

(* We prepend labels names with a dot to avoid naming collisions with
   user-defined functions. *)
let emit_label ppf label = Format.fprintf ppf ".%a" IrPP.pp_label label

let emit_constant ppf constant =
  Format.fprintf ppf ".%a" IrPP.pp_constant constant

let emit_operand ctx ppf operand =
  match operand with
  | Oreg r -> Format.fprintf ppf "%s" (name_of_reg ctx r)
  | Oframe n ->
      (* TODO: better way to compute offset, more flexible (if we support other structs or smaller integers) *)
      (* Minus because the stack is top to bottom. *)
      let offset = -(if ctx.is_x64 then n * 8 else n * 4) in
      Format.fprintf ppf "[%s + %n]" (name_of_reg ctx X86Regs.ebp) offset
  | Oimm i -> Format.fprintf ppf "%s" (Nativeint.to_string i)
  | Oconst c -> emit_constant ppf c
  | Olabel l -> emit_label ppf l
  | Ofunc f -> Format.fprintf ppf "%s" f.fn_name

let rec emit_operand_list ctx ppf operands =
  match operands with
  | [] -> ()
  | [ o ] -> emit_operand ctx ppf o
  | o :: r ->
      Format.fprintf ppf "%a, %a" (emit_operand ctx) o (emit_operand_list ctx) r

module SSet = Set.Make (String)

let x86_jmpc_insts =
  SSet.of_list [ "jz"; "jnz"; "js"; "jns"; "jc"; "jnc"; "jo"; "jno" ]

let rec emit_inst ctx ppf inst =
  match inst.i_kind with
  | "mov" -> (
      match inst.i_operands with
      (* Do not emit a mov between the same registers. *)
      | [ Oreg r1; Oreg r2 ] when r1 = r2 -> ()
      (* Use relative addressing to emit position-independent code. *)
      | [ Oreg r1; Oconst l ] ->
          Format.fprintf ppf "  lea %s, [rip + %a]\n" (name_of_reg ctx r1)
            emit_constant l
      | [ Oreg r1; Olabel l ] ->
          Format.fprintf ppf "  lea %s, [rip + %a]\n" (name_of_reg ctx r1)
            emit_label l
      | _ ->
          Format.fprintf ppf "  mov %a\n" (emit_operand_list ctx)
            inst.i_operands)
  | opname when String.starts_with opname ~prefix:"set" -> (
      match inst.i_operands with
      | [ Oreg r1 ] ->
          Format.fprintf ppf "  %s %s\n" opname (name_of_byte_reg r1)
      | _ -> failwith "invalid operands for setcc instruction")
  | "jmp" -> emit_jmp_inst ctx ppf inst
  | opname when SSet.mem opname x86_jmpc_insts -> emit_jmpc_inst ctx ppf inst
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
    | _ -> failwith "invalid operands to jmp instruction on x86"
  in
  emit_bb_or_jmp ctx ppf l

and emit_jmpc_inst ctx ppf inst =
  let then_label, else_label =
    match inst.i_operands with
    | [ Olabel tl; Olabel el ] -> (tl, el)
    | _ -> failwith "invalid operands to conditional jmp instruction on x86"
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

let emit_constant_values ppf constants =
  Hashtbl.iter
    (fun label cst ->
      Format.fprintf ppf "%a: " emit_constant label;
      match cst with
      | Ir.Icst_int s -> Format.fprintf ppf ".int %s\n" (Nativeint.to_string s)
      | Ir.Icst_string s ->
          Format.fprintf ppf ".ascii \"%s\"\n" (String.escaped s))
    constants

let emit_fn ~is_x64 ppf fn =
  emit_fn_header ppf fn.fn_name;
  let ctx = mk_ctx is_x64 fn in
  let entry_bb = Label.Map.find fn.fn_entry fn.fn_blocks in
  emit_bb ctx ppf entry_bb;
  Format.fprintf ppf "@."

let emit_ctx ~is_x64 ppf ctx funcs =
  emit_preamble ppf;

  Format.fprintf ppf ".data\n\n";
  emit_constant_values ppf ctx.Ir.ctx_constants;
  Format.fprintf ppf "\n";

  Format.fprintf ppf ".text\n\n";
  List.iter (emit_fn ~is_x64 ppf) funcs
