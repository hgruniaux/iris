(* Options to configure the default pass manager: *)
let dump_ir = ref false
let dump_ir_dot = ref false
let dump_liveinfo = ref false
let dump_interf = ref false
let dump_reg_alloc = ref false
let dump_mir = ref false
let dump_callgraph = ref false
let optimize = ref false

type pass_manager = {
  pm_arch : Backend.arch;  (** The target backend CPU architecture. *)
  pm_ir_fn_passes : (AnalysisManager.t -> Ir.fn -> bool) list;
      (** All passes that are run on IR functions. *)
  pm_mir_fn_passes : (AnalysisManager.t -> Mr.mfn -> unit) list;
      (** All passes that are run on Mr functions. *)
}

let run_pass am pass fn =
  AnalysisManager.start_pass am;
  let result = pass am fn in
  AnalysisManager.end_pass am;
  result

(** Runs the [passes] on the given [fn].

    This meta-pass returns true if at least on pass of [passes] has
    returned true. So, true is returned iff the code has changed. *)
let chain passes am fn =
  List.fold_left
    (fun changed pass -> run_pass am pass fn || changed)
    false passes

(** Repeat a given [pass] until a fixpoint is reached. In other terms,
    [pass] is executed until it returns true (code changed).

    This can be composed with [chain] to create a chain of passes that
    are run until a fixpoint is reached.

    This meta-pass returns true if at least one call to [pass] has
    changed the code; otherwise false is returned. So, true is returned
    iff the code has changed. *)
let repeat_until_fixpoint pass am fn =
  let changed = ref false in
  while run_pass am pass fn do
    changed := true
  done;
  !changed

(** Creates a conditional pass that only call [pass] on [fn] if the function
    [f] returns true. *)
let conditional_pass f pass am fn = if f () then run_pass am pass fn else false

(** Same as [conditional_pass], but takes a boolean reference instead
    of a function. *)
let ref_conditional_pass r pass am fn =
  if !r then run_pass am pass fn else false

module X86LoadParamsPass = MrLoadParamsPass.Make (X86MrBuilder)
module X86LowerCallsPass = MrLowerCallsPass.Make (X86MrBuilder)
module X86NaiveSpillerPass = MrNaiveSpillerPass.Make (X86MrBuilder)
module X86PrologEpilogPass = MrPrologEpilogPass.Make (X86MrBuilder)

let create arch =
  {
    pm_arch = arch;
    (*
      Some passes below are required for code generation
      or for other required passes. They are put between
      REQUIRED markers. DO NOT remove these passes, and be
      really sure when moving them.
    *)
    pm_ir_fn_passes =
      [
        ref_conditional_pass dump_ir (fun (_ : AnalysisManager.t) fn ->
            PPrintIr.dump_ir fn;
            false);
        ref_conditional_pass optimize
          (chain
             [
               SimplifyCFGPass.pass_fn;
               repeat_until_fixpoint
                 (chain
                    [
                      SimplifyCFGPass.pass_fn;
                      CopyPropagationPass.pass_fn;
                      InstCombinePass.pass_fn;
                      repeat_until_fixpoint DCEPass.pass_fn;
                    ]);
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
        LowerSwitchPass.pass_fn;
        SimplifyCFGPass.pass_fn;
        ref_conditional_pass dump_ir (fun _ fn ->
            PPrintIr.dump_ir fn;
            false);
        ref_conditional_pass dump_ir_dot (fun _ fn ->
            PPrintIr.dump_dot fn;
            false);
        (* BEGIN REQUIRED *)
        VerifyPass.pass_fn;
        LowerPhiPass.pass_fn;
        ref_conditional_pass dump_ir (fun _ fn ->
            PPrintIr.dump_ir fn;
            false);
        (* END REQUIRED *)
      ];
    pm_mir_fn_passes =
      [
        (* BEGIN REQUIRED *)
        X86LoadParamsPass.pass_fn;
        X86LowerCallsPass.pass_fn;
        X86NaiveSpillerPass.pass_fn;
        MrRewriteVRegsPass.pass_fn;
        X86PrologEpilogPass.pass_fn;
        (* END REQUIRED *)
      ];
  }

(** Run all registered passes of [pm] on the given [ir_fn].
    The resulting generated and optimized Mr function is
    returned. The Mr function is ready for code emitting. *)
let run_on_fn pm ctx ir_fn =
  let am = AnalysisManager.create pm.pm_arch ir_fn in

  (* Run IR passes *)
  List.iter
    (fun pass ->
      AnalysisManager.start_pass am;
      ignore (pass am ir_fn);
      AnalysisManager.end_pass am)
    pm.pm_ir_fn_passes;

  (* Instruction selection, conversion IR -> Mr *)
  let mr_fn = Backend.instsel_fn pm.pm_arch ctx ir_fn in
  am.am_mr_fun <- Some mr_fn;

  (* Run Mr passes *)
  List.iter (fun pass -> pass am mr_fn) pm.pm_mir_fn_passes;

  mr_fn

(** Runs all registered passes of [pm] on the given IR context. *)
let run_on_ctx pm out ctx =
  let callgraph = CallGraph.build ctx in

  if !dump_callgraph then CallGraph.Dot.output_graph Stdlib.stdout callgraph;

  (* Optimize functions in postfix-order of the callgraph. So, functions
     are generally optimized before being called (better for inlining passes,
     DCE and others). *)
  let mfuncs = ref [] in
  CallGraph.Dfs.postfix
    (fun fn ->
      if not fn.fn_is_external then mfuncs := run_on_fn pm ctx fn :: !mfuncs)
    callgraph;

  let formatter = Format.formatter_of_out_channel out in
  Backend.emit_ctx pm.pm_arch formatter ctx !mfuncs
