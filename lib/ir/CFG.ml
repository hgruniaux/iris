open Graph
open Ir

module G = Imperative.Digraph.Concrete (BasicBlock)

module Display = struct
  include G

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name bb = Format.asprintf "%a" PPrintIr.pp_label bb.b_label
  let vertex_attributes _ = [ `Shape `Box ]
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot (Display)
module Dfs = Graph.Traverse.Dfs (G)
module Bfs = Graph.Traverse.Bfs (G)
