(* The available registers in the x86 and x86-64 architectures. *)

(* The x86 "named" registers *)
let eax = Mir.Reg.physical 0
let ebx = Mir.Reg.physical 1
let ecx = Mir.Reg.physical 2
let edx = Mir.Reg.physical 3
let esp = Mir.Reg.physical 4
let ebp = Mir.Reg.physical 5
let esi = Mir.Reg.physical 6
let edi = Mir.Reg.physical 7
let eflags = Mir.Reg.physical 50

(* The x86-64 "named" registers *)
let rax = eax
let rbx = ebx
let rcx = ecx
let rdx = edx
let rsp = esp
let rbp = ebp
let rsi = esi
let rdi = edi
let r8 = Mir.Reg.physical 8
let r9 = Mir.Reg.physical 9
let r10 = Mir.Reg.physical 10
let r11 = Mir.Reg.physical 11
let r12 = Mir.Reg.physical 12
let r13 = Mir.Reg.physical 13
let r14 = Mir.Reg.physical 14
let r15 = Mir.Reg.physical 15
let rflags = eflags

(* Available registers in the x86 architecture. *)
let x86_registers =
  Mir.Reg.Set.of_list [ eax; ebx; ecx; edx; esp; ebp; esi; edi ]

(* Available registers in the x86-64 architecture. *)
let x64_registers =
  Mir.Reg.Set.union x86_registers
    (Mir.Reg.Set.of_list [ r8; r9; r12; r13; r14; r15 ])

(** The register used to store the return value of a function (EAX/RAX). *)
let return_reg = eax

(** The temporary register used to load and store spilled registers. *)
let spill_regs = [ r10; r11 ]

(* We use the cdecl calling convention: *)

(** The caller saved (non volatile) registers of the x86 architecture. *)
let x86_caller_saved = Mir.Reg.Set.of_list [ eax; ecx; edx ]

(** The callee saved (volatile) registers of the x86 architecture. *)
let x86_callee_saved = Mir.Reg.Set.of_list [ ebx; edi; esi; esp; ebp ]

(** The registers used to pass arguments in the x86 architecture.
    We pass all arguments by the stack. *)
let x86_args_regs = []

let x86_args_regs_count = List.length x86_args_regs

(* We the System V AMD64 ABI calling convention for x86-64: *)

(** The caller saved (non volatile) registers of the x86-64 architecture. *)
let x64_caller_saved =
  Mir.Reg.Set.of_list [ rax; rcx; rdx; rdi; rsi; r8; r9; r10; r11 ]

(** The callee saved ( volatile) registers of the x86-64 architecture. *)
let x64_callee_saved = Mir.Reg.Set.of_list [ rbx; rsp; rbp; r12; r13; r14; r15 ]

(** The registers used to pass arguments in the x86-64 architecture. *)
let x64_args_regs = [ rdi; rsi; rdx; rcx; r8; r9 ]

let x64_args_regs_count = List.length x64_args_regs

let () =
  (* Some sanity checks. The temporary register used to implement spilling
     should not be used anywhere. *)
  assert (
    List.for_all
      (fun spill_reg -> not (Mir.Reg.Set.mem spill_reg x86_registers))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (Mir.Reg.Set.mem spill_reg x64_registers))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (List.mem spill_reg x86_args_regs))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (List.mem spill_reg x64_args_regs))
      spill_regs)
