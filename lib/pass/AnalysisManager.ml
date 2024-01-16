type t = {
  am_arch : Backend.arch;
  am_ir_fun : Ir.fn;
  mutable am_mr_fun : Mr.mfn option;
  mutable am_dirty : bool;
  mutable am_in_ssa : bool;
  mutable am_regalloc : RegAlloc.coloring option;
  mutable am_liveinfo : Liveness.t option;
  mutable am_interf : Interference.graph option;
  mutable am_keep_regalloc : bool;
  mutable am_keep_liveinfo : bool;
  mutable am_keep_interf : bool;
  mutable am_keep_cfg : bool;
}

let create arch ir_fn =
  {
    am_arch = arch;
    am_ir_fun = ir_fn;
    am_mr_fun = None;
    am_dirty = false;
    am_in_ssa = true;
    am_regalloc = None;
    am_liveinfo = None;
    am_interf = None;
    am_keep_regalloc = false;
    am_keep_liveinfo = false;
    am_keep_interf = false;
    am_keep_cfg = false;
  }

let start_pass am =
  am.am_keep_regalloc <- false;
  am.am_keep_liveinfo <- false;
  am.am_keep_interf <- false;
  am.am_keep_cfg <- false

let end_pass am =
  if am.am_dirty then (
    if not am.am_keep_regalloc then am.am_regalloc <- None;
    if not am.am_keep_liveinfo then am.am_liveinfo <- None;
    if not am.am_keep_interf then am.am_interf <- None);
  am.am_dirty <- false

let run_pass am pass fn =
  start_pass am;
  pass am fn;
  end_pass am

let mark_as_dirty am = am.am_dirty <- true
let leave_ssa am = am.am_in_ssa <- false
let keep_regalloc am = am.am_keep_regalloc <- true
let keep_liveinfo am = am.am_keep_liveinfo <- true
let keep_interf am = am.am_keep_interf <- true
let keep_cfg am = am.am_keep_cfg <- true
let discard_regalloc am = am.am_keep_regalloc <- false
let discard_liveinfo am = am.am_keep_liveinfo <- false
let discard_interf am = am.am_keep_interf <- false
let discard_cfg am = am.am_keep_cfg <- false

let rec compute_liveinfo am =
  assert (Option.is_none am.am_regalloc);
  let mr_fn = Option.get am.am_mr_fun in
  let liveinfo = Liveness.compute mr_fn in
  am.am_liveinfo <- Some liveinfo;
  liveinfo

and compute_interf am =
  assert (Option.is_none am.am_interf);
  let liveinfo = liveinfo am in
  let mr_fn = Option.get am.am_mr_fun in
  let regs = Mr.collect_pseudo_registers_in_fn mr_fn in
  let interf = Interference.make regs liveinfo in
  am.am_interf <- Some interf;
  interf

and compute_regalloc am =
  assert (Option.is_none am.am_regalloc);
  let interf = interf am in
  let regalloc = RegAlloc.color am.am_arch interf in
  am.am_regalloc <- Some regalloc;
  regalloc

and liveinfo am =
  match am.am_liveinfo with
  | None -> compute_liveinfo am
  | Some liveinfo -> liveinfo

and interf am =
  match am.am_interf with None -> compute_interf am | Some interf -> interf

let regalloc am =
  match am.am_regalloc with
  | None -> compute_regalloc am
  | Some regalloc -> regalloc
