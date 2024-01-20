type arch = Arch_x86 | Arch_x64

(** Returns the list of usable registers for the given [arch]. *)
let registers arch =
  match arch with
  | Arch_x86 -> X86Regs.x86_registers
  | Arch_x64 -> X86Regs.x64_registers

(** Returns the default calling convention for [arch]. *)
let cc_info arch =
  match arch with
  | Arch_x86 -> X86Mir.x86_cc_info
  | Arch_x64 -> X86Mir.x64_cc_info

let instsel_fn arch =
  match arch with
  | Arch_x86 -> X86Instsel.instsel_fn ~is_x64:false
  | Arch_x64 -> X86Instsel.instsel_fn ~is_x64:true

let emit_fn arch =
  match arch with
  | Arch_x86 -> X86Emit.emit_fn ~is_x64:false
  | Arch_x64 -> X86Emit.emit_fn ~is_x64:true

let emit_ctx arch =
  match arch with
  | Arch_x86 -> X86Emit.emit_ctx ~is_x64:false
  | Arch_x64 -> X86Emit.emit_ctx ~is_x64:true
