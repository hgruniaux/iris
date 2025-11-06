open X64_inst

(** Name of a general-purpose register in 8-bit mode. Example, "al" for
    X64_gpr_rax. *)
let gpr_8_name = function
  | X64_gpr_rax -> "al"
  | X64_gpr_rbx -> "bl"
  | X64_gpr_rcx -> "cl"
  | X64_gpr_rdx -> "dl"
  | X64_gpr_rsi -> "sil"
  | X64_gpr_rdi -> "dil"
  | X64_gpr_rbp -> "bpl"
  | X64_gpr_rsp -> "spl"
  | X64_gpr_r8 -> "r8b"
  | X64_gpr_r9 -> "r9b"
  | X64_gpr_r10 -> "r10b"
  | X64_gpr_r11 -> "r11b"
  | X64_gpr_r12 -> "r12b"
  | X64_gpr_r13 -> "r13b"
  | X64_gpr_r14 -> "r14b"
  | X64_gpr_r15 -> "r15b"

(** Name of a general-purpose register in 16-bit mode. Example, "ax" for
    X64_gpr_rax. *)
let gpr_16_name = function
  | X64_gpr_rax -> "ax"
  | X64_gpr_rbx -> "bx"
  | X64_gpr_rcx -> "cx"
  | X64_gpr_rdx -> "dx"
  | X64_gpr_rsi -> "si"
  | X64_gpr_rdi -> "di"
  | X64_gpr_rbp -> "bp"
  | X64_gpr_rsp -> "sp"
  | X64_gpr_r8 -> "r8w"
  | X64_gpr_r9 -> "r9w"
  | X64_gpr_r10 -> "r10w"
  | X64_gpr_r11 -> "r11w"
  | X64_gpr_r12 -> "r12w"
  | X64_gpr_r13 -> "r13w"
  | X64_gpr_r14 -> "r14w"
  | X64_gpr_r15 -> "r15w"

(** Name of a general-purpose register in 32-bit mode. Example, "eax" for
    X64_gpr_rax. *)
let gpr_32_name = function
  | X64_gpr_rax -> "eax"
  | X64_gpr_rbx -> "ebx"
  | X64_gpr_rcx -> "ecx"
  | X64_gpr_rdx -> "edx"
  | X64_gpr_rsi -> "esi"
  | X64_gpr_rdi -> "edi"
  | X64_gpr_rbp -> "ebp"
  | X64_gpr_rsp -> "esp"
  | X64_gpr_r8 -> "r8d"
  | X64_gpr_r9 -> "r9d"
  | X64_gpr_r10 -> "r10d"
  | X64_gpr_r11 -> "r11d"
  | X64_gpr_r12 -> "r12d"
  | X64_gpr_r13 -> "r13d"
  | X64_gpr_r14 -> "r14d"
  | X64_gpr_r15 -> "r15d"

(** Name of a general-purpose register in 64-bit mode. Example, "rax" for
    X64_gpr_rax. *)
let gpr_64_name = function
  | X64_gpr_rax -> "rax"
  | X64_gpr_rbx -> "rbx"
  | X64_gpr_rcx -> "rcx"
  | X64_gpr_rdx -> "rdx"
  | X64_gpr_rsi -> "rsi"
  | X64_gpr_rdi -> "rdi"
  | X64_gpr_rbp -> "rbp"
  | X64_gpr_rsp -> "rsp"
  | X64_gpr_r8 -> "r8"
  | X64_gpr_r9 -> "r9"
  | X64_gpr_r10 -> "r10"
  | X64_gpr_r11 -> "r11"
  | X64_gpr_r12 -> "r12"
  | X64_gpr_r13 -> "r13"
  | X64_gpr_r14 -> "r14"
  | X64_gpr_r15 -> "r15"

let gpr_name (gpr, size) =
  match size with
  | X64_gpr_8 -> gpr_8_name gpr
  | X64_gpr_16 -> gpr_16_name gpr
  | X64_gpr_32 -> gpr_32_name gpr
  | X64_gpr_64 -> gpr_64_name gpr

let physical_reg_name pr =
  match pr with
  | X64_gpr (gpr, size) -> gpr_name (gpr, size)
  | X64_fpr _ -> failwith "X64_fpr not implemented"
