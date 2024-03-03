(* The available registers in the x86 and x86-64 architectures. *)

(* The x86 "named" registers *)
let eax = Mr.Reg.physical 0
let ebx = Mr.Reg.physical 1
let ecx = Mr.Reg.physical 2
let edx = Mr.Reg.physical 3
let esp = Mr.Reg.physical 4
let ebp = Mr.Reg.physical 5
let esi = Mr.Reg.physical 6
let edi = Mr.Reg.physical 7
let eflags = Mr.Reg.physical 50

(* The x86-64 "named" registers *)
let rax = eax
let rbx = ebx
let rcx = ecx
let rdx = edx
let rsp = esp
let rbp = ebp
let rsi = esi
let rdi = edi
let r8 = Mr.Reg.physical 8
let r9 = Mr.Reg.physical 9
let r10 = Mr.Reg.physical 10
let r11 = Mr.Reg.physical 11
let r12 = Mr.Reg.physical 12
let r13 = Mr.Reg.physical 13
let r14 = Mr.Reg.physical 14
let r15 = Mr.Reg.physical 15
let rflags = eflags

(* Available registers in the x86 architecture. *)
let x86_registers =
  Mr.Reg.Set.of_list [ eax; ebx; ecx; edx; esp; ebp; esi; edi ]

(* Available registers in the x86-64 architecture. *)
let x64_registers =
  Mr.Reg.Set.union x86_registers
    (Mr.Reg.Set.of_list [ r8; r9; r12; r13; r14; r15 ])

(** The register used to store the return value of a function (EAX/RAX). *)
let return_reg = eax

(** The temporary register used to load and store spilled registers. *)
let spill_regs = [ r10; r11 ]

(* We use the cdecl calling convention: *)

(** The caller saved (volatile) registers of the x86 architecture. *)
let x86_caller_saved = Mr.Reg.Set.of_list [ eax; ecx; edx ]

(** The callee saved (non volatile) registers of the x86 architecture. *)
let x86_callee_saved = Mr.Reg.Set.of_list [ ebx; edi; esi; esp; ebp ]

(** The registers used to pass arguments in the x86 architecture.
    We pass all arguments by the stack. *)
let x86_args_regs = []

let x86_args_regs_count = List.length x86_args_regs

(* We the System V AMD64 ABI calling convention for x86-64: *)

(** The caller saved (volatile) registers of the x86-64 architecture. *)
let x64_caller_saved = Mr.Reg.Set.of_list [ rax; rcx; rdx; rdi; rsi; r8; r9 ]

(** The callee saved (non volatile) registers of the x86-64 architecture. *)
let x64_callee_saved = Mr.Reg.Set.of_list [ rbx; rsp; rbp; r12; r13; r14; r15 ]

(** The registers used to pass arguments in the x86-64 architecture. *)
let x64_args_regs = [ rdi; rsi; rdx; rcx; r8; r9 ]

let x64_args_regs_count = List.length x64_args_regs

let () =
  (* Some sanity checks. The temporary register used to implement spilling
     should not be used anywhere. *)
  assert (
    List.for_all
      (fun spill_reg -> not (Mr.Reg.Set.mem spill_reg x86_registers))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (Mr.Reg.Set.mem spill_reg x64_registers))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (List.mem spill_reg x86_args_regs))
      spill_regs);
  assert (
    List.for_all
      (fun spill_reg -> not (List.mem spill_reg x64_args_regs))
      spill_regs)
