(* Options to configure the default pass manager: *)
let dump_ir = ref false
let dump_ir_dot = ref false
let dump_liveinfo = ref false
let dump_interf = ref false
let dump_reg_alloc = ref false
let dump_mir = ref false
let optimize = ref true

type analysis_manager = {
  mutable am_regalloc : RegAlloc.coloring option;
  mutable am_liveinfo : Liveness.t option;
  mutable am_interf : Interference.graph option;
}

type pass_manager = {
  pm_arch : Backend.arch;  (** The target backend CPU architecture. *)
  pm_am : analysis_manager;
      (** Manager that stores and handles all analysis information. *)
  pm_ir_fn_passes : (Ir.fn -> bool) list;
      (** All passes that are run on IR functions. *)
  pm_mir_fn_passes : (Mir.fn -> unit) list;
      (** All passes that are run on MIR functions. *)
}

let reg_alloc_pass am arch mir_fn =
  let liveinfo = Liveness.compute mir_fn in
  if !dump_liveinfo then Liveness.dump_liveinfo mir_fn liveinfo;
  let regs = Mir.collect_pseudo_registers_in_fn mir_fn in
  let interf = Interference.make regs liveinfo in
  if !dump_interf then Interference.dump_interference interf;
  let regalloc = RegAlloc.color arch interf in
  if !dump_reg_alloc then RegAlloc.dump_colors regalloc;
  (* Save info in the Analysis Manager *)
  am.am_liveinfo <- Some liveinfo;
  am.am_interf <- Some interf;
  am.am_regalloc <- Some regalloc

(** Creates a new pass that call [pass] on [fn] but with
    liveinfo analysis information as first argument. *)
let pass_with_liveinfo am pass fn = pass (Option.get am.am_liveinfo) fn

(** Creates a new pass that call [pass] on [fn] but with
    interference graph analysis information as first argument. *)
let pass_with_interf am pass fn = pass (Option.get am.am_interf) fn

(** Creates a new pass that call [pass] on [fn] but with
    register allocation information as first argument. *)
let pass_with_regalloc am pass fn = pass (Option.get am.am_regalloc) fn

(** Runs the [passes] on the given [fn].

    This meta-pass returns true if at least on pass of [passes] has
    returned true. So, true is returned iff the code has changed. *)
let chain passes fn =
  List.fold_left (fun changed pass -> pass fn || changed) false passes

(** Repeat a given [pass] until a fixpoint is reached. In other terms,
    [pass] is executed until it returns true (code changed).

    This can be composed with [chain] to create a chain of passes that
    are run until a fixpoint is reached.

    This meta-pass returns true if at least one call to [pass] has
    changed the code; otherwise false is returned. So, true is returned
    iff the code has changed. *)
let repeat_until_fixpoint pass fn =
  let changed = ref false in
  while pass fn do
    changed := true
  done;
  !changed

(** Creates a conditional pass that only call [pass] on [fn] if the function
    [f] returns true. *)
let conditional_pass f pass fn = if f () then pass fn else false

(** Same as [conditional_pass], but takes a boolean reference instead
    of a function. *)
let ref_conditional_pass r pass fn = if !r then pass fn else false

(** Act as a pass that does nothing. It completly ignore the given [pass].

    This is usefull for debugging and experimenting purposes to easily
    disable a pass.

    This meta-pass always returns false as it never changes the code. *)
let ignore_pass pass fn =
  ignore pass;
  ignore fn;
  false

(** Creates an analysis manager. *)
let create_am () = { am_liveinfo = None; am_interf = None; am_regalloc = None }

let create arch =
  let am = create_am () in
  {
    pm_arch = arch;
    pm_am = am;
    (*
      Some passes below are required for code generation
      or for other required passes. They are put between
      REQUIRED markers. DO NOT remove these passes, and be
      really sure when moving them.
    *)
    pm_ir_fn_passes =
      [
        ref_conditional_pass optimize
          (chain
             [
               SimplifyCFGPass.pass_fn;
               MergeRetPass.pass_fn;
               repeat_until_fixpoint
                 (chain
                    [
                      SimplifyCFGPass.pass_fn;
                      CopyPropagationPass.pass_fn;
                      InstCombinePass.pass_fn;
                      repeat_until_fixpoint DCEPass.pass_fn;
                    ]);
             ]);
        ref_conditional_pass dump_ir (fun fn ->
            IrPP.dump_ir fn;
            false);
        ref_conditional_pass dump_ir_dot (fun fn ->
            IrPP.dump_dot fn;
            false);
        (* BEGIN REQUIRED *)
        VerifyPass.pass_fn;
        LowerPhiPass.pass_fn;
        ref_conditional_pass dump_ir (fun fn ->
            IrPP.dump_ir fn;
            false);
        (* END REQUIRED *)
      ];
    pm_mir_fn_passes =
      [
        (* BEGIN REQUIRED *)
        LoadParamsPass.pass_fn arch;
        LowerCallsPass.pass_fn arch;
        (* (fun fn -> Mir.dump_mir [fn]); *)
        reg_alloc_pass am arch;
        pass_with_regalloc am (NaiveSpillerPass.pass_fn X86Regs.spill_regs);
        pass_with_regalloc am RewriteVRegsPass.pass_fn;
        PrologEpilogPass.pass_fn arch;
        (* END REQUIRED *)
      ];
  }

(** Run all registered passes of [pm] on the given [ir_fn].
    The resulting generated and optimized MIR function is
    returned. The MIR function is ready for code emitting. *)
let run pm ir_fn =
  (* Run IR passes *)
  List.iter (fun pass -> ignore (pass ir_fn)) pm.pm_ir_fn_passes;

  (* Instruction selection, conversion IR -> MIR *)
  let mir_fn = Backend.instsel_fn pm.pm_arch ir_fn in

  (* Run MIR passes *)
  List.iter (fun pass -> pass mir_fn) pm.pm_mir_fn_passes;

  mir_fn
