open Ir_base
module Block = Ir_block
module Function = Ir_function

module Cfg = struct
  type t = fn

  module V = struct
    type t = bb

    let hash b = Label.hash (Block.label b)
    let equal b1 b2 = Label.equal (Block.label b1) (Block.label b2)
    let compare b1 b2 = Label.compare (Block.label b1) (Block.label b2)
  end

  module E = struct
    type t = bb * bb

    let src (b1, _) = b1
    let dst (_, b2) = b2
  end

  let is_directed = true

  let resolve_labels fn labels =
    LabelSet.fold (fun l acc -> LabelMap.find l fn.fn_blocks :: acc) labels []

  let pred fn block = Block.pred block |> resolve_labels fn
  let pred_e fn block = List.map (fun b -> (b, block)) (pred fn block)
  let succ fn block = Block.succ block |> resolve_labels fn
  let succ_e fn block = List.map (fun b -> (block, b)) (succ fn block)

  let fold_vertex f fn acc =
    LabelMap.fold (fun _ block acc -> f block acc) fn.fn_blocks acc

  let iter_vertex f fn = LabelMap.iter (fun _ block -> f block) fn.fn_blocks
  let nb_vertex fn = LabelMap.cardinal fn.fn_blocks

  let iter_edges f fn =
    iter_vertex
      (fun block ->
        List.iter (fun succ_bb -> f (block, succ_bb)) (succ fn block))
      fn

  let fold_succ f fn block acc =
    succ fn block |> List.fold_left (fun acc b -> f b acc) acc

  let iter_succ f fn block = succ fn block |> List.iter (fun b -> f b)

  let fold_pred f fn block acc =
    pred fn block |> List.fold_left (fun acc b -> f b acc) acc

  let fold_pred_e f fn block acc =
    pred_e fn block |> List.fold_left (fun acc e -> f e acc) acc

  let iter_pred f fn block = pred fn block |> List.iter (fun b -> f b)
end

module Dominator = Graph.Dominator.Make (Cfg)

module Fixpoint
    (A :
      Graph.Fixpoint.Analysis
        with type g = Cfg.t
         and type vertex = Cfg.V.t
         and type edge = Cfg.E.t) =
  Graph.Fixpoint.Make (Cfg) (A)

module ChaoticIteration = Graph.ChaoticIteration.Make (Cfg)
module WeakTopological = Graph.WeakTopological.Make (Cfg)
module Leaderlist = Graph.Leaderlist.Make (Cfg)
module Dfs = Graph.Traverse.Dfs (Cfg)
module Bfs = Graph.Traverse.Bfs (Cfg)
include Cfg

let compute_idom fn =
  let entry_block = Option.get (Function.entry_block fn) in
  Dominator.compute_idom fn entry_block
