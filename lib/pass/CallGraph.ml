open Graph

(* Static call graph of a IR context. It represents the calling relationships
   between subroutines in the program:
     - the vertices are the Ir.fn instances of the context.
     - the edges are the caller -> callee calling relationship. *)

(* FIXME: Add "used but not called" edges. For example, if the function's
          address is taken. It is important to add such edges to know when a
          function is needed or not.

          This can be easily implemented as an additional label to edges
          that specify the edge kind. *)

module Function = struct
  type t = Ir.fn

  let compare a b = Stdlib.compare a.Ir.fn_name b.Ir.fn_name
  let equal a b = a.Ir.fn_name = b.Ir.fn_name
  let hash fn = Hashtbl.hash fn.Ir.fn_name
end

module G = Imperative.Digraph.Concrete (Function)
include G

module Display = struct
  include G

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name fn = fn.Ir.fn_name

  let vertex_attributes fn =
    if fn.Ir.fn_is_external then [ `Shape `Diamond ] else [ `Shape `Box ]

  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot (Display)
module Dfs = Graph.Traverse.Dfs (G)
module Bfs = Graph.Traverse.Bfs (G)

(** Builds the call graph of the given [ctx]. *)
let build ctx =
  let g = create () in

  List.iter
    (fun caller ->
      (* Add caller to the call graph unconditionally. So, all functions
         are always included in the call graph even it they are never called. *)
      add_vertex g caller;

      Ir.Label.Map.iter
        (fun _ bb ->
          List.iter
            (fun inst ->
              match inst.Ir.i_kind with
              | Ir.Iinst_call (callee, _) -> add_edge g caller callee
              | _ -> ())
            bb.Ir.b_insts)
        caller.Ir.fn_blocks)
    ctx.Ir.ctx_funcs;
  g
