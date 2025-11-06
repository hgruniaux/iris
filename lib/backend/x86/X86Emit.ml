open Mr
open Format

type section = Unknown | Text | Data | Rodata

let current_section = ref Unknown

type emit_ctx = {
  is_x64 : bool;
  cur_fn : Mr.mfn;
  ir_ctx : Ir.ctx;
  mutable already_emitted_bbs : LabelSet.t;
}

let mk_ctx is_x64 ir_ctx cur_fn =
  { is_x64; cur_fn; ir_ctx; already_emitted_bbs = LabelSet.empty }

let switch_section ppf new_section =
  if !current_section <> new_section then (
    current_section := new_section;
    match new_section with
    | Unknown -> ()
    | Text -> fprintf ppf ".text\n"
    | Data -> fprintf ppf ".data\n"
    | Rodata -> fprintf ppf ".rodata\n")

let resolve_label ctx l = LabelMap.find l ctx.cur_fn.mfn_blocks
let is_bb_already_emitted ctx l = LabelSet.mem l ctx.already_emitted_bbs
let emit_preamble ppf = fprintf ppf ".intel_syntax noprefix\n\n"

let emit_fn_header ppf fname =
  fprintf ppf ".globl %s\n" fname;
  fprintf ppf ".type %s, @@function\n" fname;
  fprintf ppf "%s:\n" fname

let x86_reg_names =
  List.fold_left
    (fun m (r, name) -> RegMap.add r name m)
    RegMap.empty
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
    (fun m (r, name) -> RegMap.add r name m)
    RegMap.empty
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
  if ctx.is_x64 then RegMap.find r x64_reg_names
  else RegMap.find r x86_reg_names

let byte_reg_names =
  List.fold_left
    (fun m (r, name) -> RegMap.add r name m)
    RegMap.empty
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

let name_of_byte_reg r = RegMap.find r byte_reg_names

(* We prepend labels names with a dot to avoid naming collisions with
   user-defined functions. *)
let emit_label =
  let labels = Hashtbl.create 16 in
  fun fn ppf label ->
    match Hashtbl.find_opt labels (label, fn) with
    | Some id -> fprintf ppf ".L%d" id
    | None ->
        let id = Hashtbl.length labels in
        Hashtbl.add labels (label, fn) id;
        fprintf ppf ".L%d" id

let emit_global_ref =
  let unnamed_globals = Hashtbl.create 16 in
  fun ppf global ->
    match global.Ir.global_name with
    | Some name -> fprintf ppf "%s" name
    | None ->
        let id =
          try Hashtbl.find unnamed_globals global
          with Not_found ->
            let new_id = Hashtbl.length unnamed_globals in
            Hashtbl.add unnamed_globals global new_id;
            new_id
        in
        fprintf ppf ".LC%d" id

let emit_operand ctx ppf operand =
  match operand with
  | Oreg r -> fprintf ppf "%s" (name_of_reg ctx r)
  | Oframe n ->
      (* TODO: better way to compute offset, more flexible (if we support other structs or smaller integers) *)
      (* Minus because the stack is top to bottom. *)
      let n = n + 1 in
      let offset = if ctx.is_x64 then n * 8 else n * 4 in
      fprintf ppf "[%s - %n]" (name_of_reg ctx X86Regs.ebp) offset
  | Oimm i -> fprintf ppf "%s" (Z.to_string i)
  | Oglobal g -> emit_global_ref ppf g
  | Olabel l -> (emit_label ctx.cur_fn) ppf l
  | Ofunc f -> fprintf ppf "%s" f.fn_name
  | Omem (base, shift, offset) ->
      fprintf ppf "[%d * %s + %d]" shift (name_of_reg ctx base) offset

let emit_operand_list ctx ppf operands =
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf ", ")
    (emit_operand ctx) ppf operands

module SSet = Set.Make (String)

let x86_jmpc_insts =
  SSet.of_list [ "jz"; "jnz"; "js"; "jns"; "jc"; "jnc"; "jo"; "jno" ]

let rec emit_inst ctx ppf inst =
  match inst.mi_kind with
  | "mov" -> (
      match inst.mi_operands with
      (* Do not emit a mov between the same registers. *)
      | [ Oreg r1; Oreg r2 ] when r1 = r2 -> ()
      (* Use relative addressing to emit position-independent code. *)
      | [ Oreg r1; Oglobal g ] ->
          fprintf ppf "  lea %s, [rip + %a]\n" (name_of_reg ctx r1)
            emit_global_ref g
      | [ Oreg r1; Olabel l ] ->
          fprintf ppf "  lea %s, [rip + %a]\n" (name_of_reg ctx r1)
            (emit_label ctx.cur_fn) l
      | _ -> fprintf ppf "  mov %a\n" (emit_operand_list ctx) inst.mi_operands)
  | opname when String.starts_with opname ~prefix:"set" -> (
      match inst.mi_operands with
      | [ Oreg r1 ] -> fprintf ppf "  %s %s\n" opname (name_of_byte_reg r1)
      | _ -> failwith "invalid operands for setcc instruction")
  | "jmp" -> emit_jmp_inst ctx ppf inst
  | opname when SSet.mem opname x86_jmpc_insts -> emit_jmpc_inst ctx ppf inst
  | opname ->
      fprintf ppf "  %s %a\n" opname (emit_operand_list ctx) inst.mi_operands

and emit_bb_or_jmp ctx ppf l =
  if is_bb_already_emitted ctx l then
    fprintf ppf "  jmp %a\n" (emit_label ctx.cur_fn) l
  else
    let target_bb = resolve_label ctx l in
    emit_bb ctx ppf target_bb

and emit_jmp_inst ctx ppf inst =
  let l =
    match inst.mi_operands with
    | [ Olabel l ] -> l
    | _ -> failwith "invalid operands to jmp instruction on x86"
  in
  emit_bb_or_jmp ctx ppf l

and emit_jmpc_inst ctx ppf inst =
  let then_label, else_label =
    match inst.mi_operands with
    | [ Olabel tl; Olabel el ] -> (tl, el)
    | _ -> failwith "invalid operands to conditional jmp instruction on x86"
  in
  fprintf ppf "  %s %a\n" inst.mi_kind (emit_label ctx.cur_fn) then_label;
  emit_bb_or_jmp ctx ppf else_label;
  emit_bb ctx ppf (resolve_label ctx then_label)

and emit_bb ctx ppf bb =
  if is_bb_already_emitted ctx bb.mbb_label then ()
  else (
    ctx.already_emitted_bbs <- LabelSet.add bb.mbb_label ctx.already_emitted_bbs;
    fprintf ppf "%a:\n" (emit_label ctx.cur_fn) bb.mbb_label;
    List.iter (emit_inst ctx ppf) bb.mbb_insts)

let emit_constant_value ppf typ constant =
  match constant with
  | Ir.Iconstant_uninitialized -> ()
  | Ir.Iconstant_bytes bytes ->
      fprintf ppf ".ascii \"%a\"\n" pp_print_bytes (Bytes.escaped bytes)
  | Ir.Iconstant_int i -> (
      match typ with
      | Ir.Ityp_i8 -> fprintf ppf ".byte %a\n" Z.pp_print i
      | Ir.Ityp_i16 -> fprintf ppf ".word %a\n" Z.pp_print i
      | Ir.Ityp_i32 -> fprintf ppf ".long %a\n" Z.pp_print i
      | Ir.Ityp_i64 -> fprintf ppf ".int %a\n" Z.pp_print i
      | _ -> assert false)
  | Ir.Iconstant_float _ -> (
      (* FIXME: Add floating-point support *)
      match typ with
      | _ -> failwith "Unsupported float constant type")

let emit_global_variable ppf global typ initial_value =
  let is_external = Option.is_some global.Ir.global_name in
  if is_external then fprintf ppf ".globl %a\n" emit_global_ref global;
  if global.global_mutable then switch_section ppf Data
  else switch_section ppf Rodata;
  fprintf ppf "%a:\n" emit_global_ref global;
  emit_constant_value ppf typ initial_value;
  fprintf ppf ".type %a, @object" emit_global_ref global

let emit_fn ~is_x64 ppf ir_ctx fn =
  switch_section ppf Text;
  emit_fn_header ppf fn.mfn_name;
  let ctx = mk_ctx is_x64 ir_ctx fn in
  let entry_bb = LabelMap.find fn.mfn_entry fn.mfn_blocks in
  emit_bb ctx ppf entry_bb;
  fprintf ppf "\n"

let emit_globals ppf globals =
  List.iter
    (fun global ->
      match global.Ir.global_kind with
      | Ir.Iglobal_variable (typ, initial_value) ->
          emit_global_variable ppf global typ initial_value;
          fprintf ppf "\n\n"
      | Ir.Iglobal_function _ -> ())
    globals

let emit_ctx ~is_x64 ppf ir_mod funcs =
  emit_preamble ppf;

  emit_globals ppf ir_mod.Ir.mod_globals;
  List.iter (emit_fn ~is_x64 ppf ir_mod) funcs;
  fprintf ppf "@."
