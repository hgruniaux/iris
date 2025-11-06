(*
 * This module defines the interference graph and the function that creates it
 * from the liveness information.
 *)

open Mr
open Liveness

type edge_kind = Interf | Pref

module EdgeLabel = struct
  type t = edge_kind

  let default = Interf
  let compare = Stdlib.compare
end

module G = Graph.Imperative.Graph.ConcreteLabeled (Reg) (EdgeLabel)

type arcs = { prefs : RegSet.t; intfs : RegSet.t }
type graph = arcs RegMap.t

let find_or_create_arc g r =
  match RegMap.find_opt r g with
  | Some a -> a
  | None -> { prefs = RegSet.empty; intfs = RegSet.empty }

(** Adds a preference (move-related) edge between [r1] and [r2] in the graph
    [g]. *)
let add_pref g r1 r2 =
  if r1 = r2 then g
  else
    let arc1 = find_or_create_arc g r1 in
    let arc2 = find_or_create_arc g r2 in
    (* If both registers interfer then do not add a preference edge. *)
    if RegSet.mem r2 arc1.intfs || RegSet.mem r1 arc2.intfs then g
    else
      let new_arc1 = { prefs = RegSet.add r2 arc1.prefs; intfs = arc1.intfs } in
      let new_arc2 = { prefs = RegSet.add r1 arc2.prefs; intfs = arc2.intfs } in
      let g = if Reg.is_pseudo r2 then RegMap.add r2 new_arc2 g else g in
      if Reg.is_pseudo r1 then RegMap.add r1 new_arc1 g else g

(** Adds an interference edge between [r1] and [r2] in the graph [g]. *)
let add_intf g r1 r2 =
  if r1 = r2 then g
  else
    let arc1 = find_or_create_arc g r1 in
    let arc2 = find_or_create_arc g r2 in
    let new_arc1 =
      { prefs = RegSet.remove r2 arc1.prefs; intfs = RegSet.add r2 arc1.intfs }
    in
    let new_arc2 =
      { prefs = RegSet.remove r1 arc2.prefs; intfs = RegSet.add r1 arc2.intfs }
    in
    let g = if Reg.is_pseudo r2 then RegMap.add r2 new_arc2 g else g in
    if Reg.is_pseudo r1 then RegMap.add r1 new_arc1 g else g

(** Removes the register [v] from the interference graph [g]. *)
let remove g v =
  match RegMap.find_opt v g with
  | None -> g (* nothing to remove! *)
  | Some a ->
      (* Removes all the edges connected to v. *)
      let g =
        RegSet.fold
          (fun w g ->
            let a = find_or_create_arc g w in
            let new_a =
              {
                intfs = RegSet.remove v a.intfs;
                prefs = RegSet.remove v a.prefs;
              }
            in
            RegMap.add w new_a g)
          (RegSet.union a.intfs a.prefs)
          g
      in
      (* Effectively removes the vertex from the graph. *)
      RegMap.remove v g

(** Makes the interference graph from [liveinfo]. *)
let make regs liveinfo =
  (* The algorithm comes from Modern Compiler Implementation in ML, § 11.4. *)
  let graph =
    ref
      (RegSet.fold
         (fun reg graph ->
           RegMap.add reg { prefs = RegSet.empty; intfs = RegSet.empty } graph)
         regs RegMap.empty)
  in

  Hashtbl.iter
    (fun bb bb_liveinfo ->
      let live = ref bb_liveinfo.bb_live_out in
      List.iter
        (fun inst ->
          if inst.mi_is_mov then (
            (* Add a special preference edge for mov instructions so the registers
               can be coalesced later. *)
            live := RegSet.diff !live inst.mi_defs;

            RegSet.iter
              (fun d ->
                RegSet.iter (fun u -> graph := add_pref !graph d u) inst.mi_uses)
              inst.mi_defs);

          live := RegSet.union !live inst.mi_defs;

          RegSet.iter
            (fun d -> RegSet.iter (fun r -> graph := add_intf !graph d r) !live)
            inst.mi_defs;

          live := RegSet.union inst.mi_uses (RegSet.diff !live inst.mi_defs))
        (List.rev bb.mbb_insts))
    liveinfo;

  !graph

let dump_interference graph =
  let vertices = Hashtbl.create 17 in
  let intfs_edges = Hashtbl.create 17 in
  let prefs_edges = Hashtbl.create 17 in

  let pp_vertex ppf vertex =
    if not (Hashtbl.mem vertices vertex) then (
      if Reg.is_physical vertex then
        Format.fprintf ppf "_%d [label=\"%a\";shape=rect];\n" (Reg.id vertex)
          Reg.pp_print vertex
      else
        Format.fprintf ppf "_%d [label=\"%a\";shape=circle];\n" (Reg.id vertex)
          Reg.pp_print vertex;
      Hashtbl.add vertices vertex true)
  in

  let pp_intf_edge ppf u v =
    if not (Hashtbl.mem intfs_edges (u, v) || Hashtbl.mem intfs_edges (v, u))
    then (
      Format.fprintf ppf "_%d -- _%d;\n" (Reg.id u) (Reg.id v);
      Hashtbl.add intfs_edges (u, v) true)
  in

  let pp_pref_edge ppf u v =
    if not (Hashtbl.mem prefs_edges (u, v) || Hashtbl.mem prefs_edges (v, u))
    then (
      Format.fprintf ppf "_%d -- _%d [style=dashed];\n" (Reg.id u) (Reg.id v);
      Hashtbl.add prefs_edges (u, v) true)
  in

  let pp_graph ppf graph =
    RegMap.iter
      (fun r1 arc ->
        pp_vertex ppf r1;

        RegSet.iter
          (fun r2 ->
            pp_vertex ppf r2;
            pp_intf_edge ppf r1 r2)
          arc.intfs;

        RegSet.iter
          (fun r2 ->
            pp_vertex ppf r2;
            pp_pref_edge ppf r1 r2)
          arc.prefs)
      graph
  in

  Format.printf "graph Interference {\nlayout=circo;\n%a}@." pp_graph graph
