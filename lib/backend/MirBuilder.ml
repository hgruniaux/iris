open Backend
open Mir

let mk_mov arch target source =
  match arch with
  | Arch_x86 | Arch_x64 -> X86Mir.mk_mov target source
  | Arch_cpulm -> CPUlmMir.mk_mov target source

let mk_mov_operand arch target operand =
  match arch with
  | Arch_x86 | Arch_x64 -> X86Mir.mk_mov_operand target operand
  | Arch_cpulm -> CPUlmMir.mk_mov_operand target operand

let mk_push arch source =
  match arch with
  | Arch_x86 | Arch_x64 -> X86Mir.mk_push source
  | Arch_cpulm -> CPUlmMir.mk_push source

let mk_push_operand arch operand =
  match operand with
  | Oreg r -> mk_push arch r
  | _ -> failwith "TODO: support non register push"

let mk_pop arch target =
  match arch with
  | Arch_x86 | Arch_x64 -> X86Mir.mk_pop target
  | Arch_cpulm -> CPUlmMir.mk_pop target

let mk_pop_many arch count =
  if count = 0 then []
  else
    match arch with
    | Arch_x86 | Arch_x64 -> X86Mir.mk_pop_many arch count
    | Arch_cpulm -> CPUlmMir.mk_pop_many count
