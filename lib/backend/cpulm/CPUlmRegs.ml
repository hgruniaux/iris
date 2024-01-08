(* The available registers in the CPUlm. *)

let r0 = Mir.Reg.physical 0
let r1 = Mir.Reg.physical 1
let r2 = Mir.Reg.physical 2
let r3 = Mir.Reg.physical 3
let r4 = Mir.Reg.physical 4
let r5 = Mir.Reg.physical 5
let r6 = Mir.Reg.physical 6
let r7 = Mir.Reg.physical 7
let r8 = Mir.Reg.physical 8
let r9 = Mir.Reg.physical 9
let r10 = Mir.Reg.physical 10
let r11 = Mir.Reg.physical 11
let r12 = Mir.Reg.physical 12
let r13 = Mir.Reg.physical 13
let r14 = Mir.Reg.physical 14
let r15 = Mir.Reg.physical 15
let r16 = Mir.Reg.physical 16
let r17 = Mir.Reg.physical 17
let r18 = Mir.Reg.physical 18
let r19 = Mir.Reg.physical 19
let r20 = Mir.Reg.physical 20
let r21 = Mir.Reg.physical 21
let r22 = Mir.Reg.physical 22
let r23 = Mir.Reg.physical 23
let r24 = Mir.Reg.physical 24
let r25 = Mir.Reg.physical 25
let r26 = Mir.Reg.physical 26
let r27 = Mir.Reg.physical 27
let r28 = Mir.Reg.physical 28
let rout = Mir.Reg.physical 29
let rsp = Mir.Reg.physical 30
let rpriv = Mir.Reg.physical 31

(** The available registers for the CPUlm architecture. *)
let registers = Mir.Reg.Set.of_list
  [
    r2;
    r3;
    r4;
    r5;
    r6;
    r7;
    r8;
    r9;
    r10;
    r11;
    r12;
    r13;
    r14;
    r15;
    r16;
    r17;
    r18;
    r19;
    r20;
    r21;
    r22;
    r23;
    r24;
    r25;
    r26;
    r27;
    (* r28 reserved for spilling *)
    rout;
    rsp;
    (* we do not use rpriv *)
  ]

(** The register used to store the return value of a function (rout). *)
let return_reg = rout

(** The temporary register used to load and store spilled registers. *)
let spill_reg = r28

(** The caller saved (non volatile) registers of the CPUlm architecture. *)
let caller_saved =
  Mir.Reg.Set.of_list
    [ r15; r16; r18; r19; r20; r21; r22; r23; r24; r25; r26; r27; rout ]

(** The callee saved (volatile) registers of the CPUlm architecture. *)
let callee_saved =
  Mir.Reg.Set.of_list
    [ r2; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; rsp; rpriv ]

(** The registers used to pass arguments in the CPUlm architecture. *)
let args_regs = [ r20; r21; r22; r23; r24; r25; r26; r27 ]

let args_regs_count = List.length args_regs

let () =
  (* Some sanity checks. The temporary register used to implement spilling
     should not be used anywhere. *)
  assert (not (Mir.Reg.Set.mem spill_reg registers));
  assert (not (List.mem spill_reg args_regs))
