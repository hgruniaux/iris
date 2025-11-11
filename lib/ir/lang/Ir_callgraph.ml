open Ir_base
module BasicBlock = Ir_block
module Function = Ir_function
module Module = Ir_module

module CallGraph = struct
  type t = {
    functions : fn list;  (** All functions in the module. *)
    callers : (fn, fn list) Hashtbl.t;
        (** The functions that call a given function. *)
    callees : (fn, fn list) Hashtbl.t;
        (** The functions that are called by a given function. *)
  }

  let _register_call cg caller callee =
    let update_table tbl key value =
      let existing =
        match Hashtbl.find_opt tbl key with None -> [] | Some v -> v
      in
      Hashtbl.replace tbl key (value :: existing)
    in
    update_table cg.callees caller callee;
    update_table cg.callers callee caller

  let _register_call_inst cg caller callee =
    match callee with
    | Ival_global g -> (
        match g.global_kind with
        | Iglobal_function callee_fn -> _register_call cg caller callee_fn
        | _ -> ())
    | _ -> ()

  let _iter_function cg f =
    Function.iter_blocks
      (fun bb ->
        BasicBlock.iter_insts
          (fun inst ->
            match inst with
            | Iinst_def (_, Iexpr_call (callee, _)) ->
                _register_call_inst cg f callee
            | _ -> ())
          bb)
      f;
    ignore cg

  let compute ir_module =
    let functions =
      Module.fold_functions (fun fn acc -> fn :: acc) ir_module []
    in
    let cg =
      { functions; callers = Hashtbl.create 17; callees = Hashtbl.create 17 }
    in
    List.iter (_iter_function cg) functions;
    cg

  module V = struct
    type t = fn

    let hash fn = Hashtbl.hash fn.fn_name
    let equal fn1 fn2 = String.equal fn1.fn_name fn2.fn_name
    let compare fn1 fn2 = String.compare fn1.fn_name fn2.fn_name
  end

  module E = struct
    type t = fn * fn

    let src (f1, _) = f1
    let dst (_, f2) = f2
  end

  let is_directed = true

  let succ cg fn =
    match Hashtbl.find_opt cg.callees fn with
    | None -> []
    | Some callees -> callees

  let succ_e cg fn =
    match Hashtbl.find_opt cg.callees fn with
    | None -> []
    | Some callees -> List.map (fun callee -> (fn, callee)) callees

  let iter_succ f cg fn =
    match Hashtbl.find_opt cg.callees fn with
    | None -> ()
    | Some callees -> List.iter f callees

  let fold_succ f cg fn acc =
    match Hashtbl.find_opt cg.callees fn with
    | None -> acc
    | Some callees ->
        List.fold_left (fun acc callee -> f callee acc) acc callees

  let pred cg fn =
    match Hashtbl.find_opt cg.callers fn with
    | None -> []
    | Some callers -> callers

  let pred_e cg fn =
    match Hashtbl.find_opt cg.callers fn with
    | None -> []
    | Some callers -> List.map (fun caller -> (caller, fn)) callers

  let iter_pred f cg fn =
    match Hashtbl.find_opt cg.callers fn with
    | None -> ()
    | Some callers -> List.iter f callers

  let fold_pred f cg fn acc =
    match Hashtbl.find_opt cg.callers fn with
    | None -> acc
    | Some callers ->
        List.fold_left (fun acc caller -> f caller acc) acc callers

  let iter_vertex f cg = List.iter f cg.functions

  let fold_vertex f cg acc =
    List.fold_left (fun acc fn -> f fn acc) acc cg.functions

  let nb_vertex cg = List.length cg.functions

  let iter_edges f cg =
    List.iter
      (fun caller ->
        match Hashtbl.find_opt cg.callees caller with
        | None -> ()
        | Some callees -> List.iter (fun callee -> f (caller, callee)) callees)
      cg.functions
end

module Dominator = Graph.Dominator.Make (CallGraph)
module Fixpoint = Graph.Fixpoint.Make (CallGraph)
module Dfs = Graph.Traverse.Dfs (CallGraph)
module Bfs = Graph.Traverse.Bfs (CallGraph)
include CallGraph
