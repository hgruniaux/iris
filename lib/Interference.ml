(*
 * This module defines the interference graph and the function that creates it
 * from the liveness information.
 *)

open Mir
open Liveness

type arcs = { prefs : Reg.set; intfs : Reg.set }
type graph = arcs Reg.map

let find_or_create_arc g r =
  match Reg.Map.find_opt r g with
  | Some a -> a
  | None -> { prefs = Reg.Set.empty; intfs = Reg.Set.empty }

(** Adds a preference (move-related) edge between [r1] and [r2] in the graph [g]. *)
let add_pref g r1 r2 =
  if r1 = r2 then g
  else
    let arc1 = find_or_create_arc g r1 in
    let arc2 = find_or_create_arc g r2 in
    (* If both registers interfer then do not add a preference edge. *)
    if Reg.Set.mem r2 arc1.intfs || Reg.Set.mem r1 arc2.intfs then g
    else
      let new_arc1 =
        { prefs = Reg.Set.add r2 arc1.prefs; intfs = arc1.intfs }
      in
      let new_arc2 =
        { prefs = Reg.Set.add r1 arc2.prefs; intfs = arc2.intfs }
      in
      let g = if Reg.is_pseudo r2 then Reg.Map.add r2 new_arc2 g else g in
      if Reg.is_pseudo r1 then Reg.Map.add r1 new_arc1 g else g

(** Adds an interference edge between [r1] and [r2] in the graph [g]. *)
let add_intf g r1 r2 =
  if r1 = r2 then g
  else
    let arc1 = find_or_create_arc g r1 in
    let arc2 = find_or_create_arc g r2 in
    let new_arc1 =
      {
        prefs = Reg.Set.remove r2 arc1.prefs;
        intfs = Reg.Set.add r2 arc1.intfs;
      }
    in
    let new_arc2 =
      {
        prefs = Reg.Set.remove r1 arc2.prefs;
        intfs = Reg.Set.add r1 arc2.intfs;
      }
    in
    let g = if Reg.is_pseudo r2 then Reg.Map.add r2 new_arc2 g else g in
    if Reg.is_pseudo r1 then Reg.Map.add r1 new_arc1 g else g

(** Removes the register [v] from the interference graph [g]. *)
let remove g v =
  match Reg.Map.find_opt v g with
  | None -> g (* nothing to remove! *)
  | Some a ->
      (* Removes all the edges connected to v. *)
      let g =
        Reg.Set.fold
          (fun w g ->
            let a = find_or_create_arc g w in
            let new_a =
              {
                intfs = Reg.Set.remove v a.intfs;
                prefs = Reg.Set.remove v a.prefs;
              }
            in
            Reg.Map.add w new_a g)
          (Reg.Set.union a.intfs a.prefs)
          g
      in
      (* Effectively removes the vertex from the graph. *)
      Reg.Map.remove v g

(** Makes the interference graph from [liveinfo]. *)
let make regs liveinfo =
  (* The algorithm comes from Modern Compiler Implementation in ML, § 11.4. *)
  let graph =
    ref
      (Reg.Set.fold
         (fun reg graph ->
           Reg.Map.add reg
             { prefs = Reg.Set.empty; intfs = Reg.Set.empty }
             graph)
         regs Reg.Map.empty)
  in

  Hashtbl.iter
    (fun bb bb_liveinfo ->
      let live = ref bb_liveinfo.bb_live_out in
      List.iter
        (fun inst ->
          if inst.i_is_mov then (
            (* Add a special preference edge for mov instructions so the registers
               can be coalesced later. *)
            live := Reg.Set.diff !live inst.i_defs;

            Reg.Set.iter
              (fun d ->
                Reg.Set.iter (fun u -> graph := add_pref !graph d u) inst.i_uses)
              inst.i_defs);

          live := Reg.Set.union !live inst.i_defs;

          Reg.Set.iter
            (fun d ->
              Reg.Set.iter (fun r -> graph := add_intf !graph d r) !live)
            inst.i_defs;

          live := Reg.Set.union inst.i_uses (Reg.Set.diff !live inst.i_defs))
        (List.rev bb.bb_insts))
    liveinfo;

  !graph

let dump_interference graph =
  let vertices = Hashtbl.create 17 in
  let intfs_edges = Hashtbl.create 17 in
  let prefs_edges = Hashtbl.create 17 in

  let pp_vertex ppf vertex =
    if not (Hashtbl.mem vertices vertex) then (
      if Reg.is_physical vertex then
        Format.fprintf ppf "_%d [label=\"%a\";shape=rect];\n" vertex
          IrPrettyPrinter.pp_register vertex
      else
        Format.fprintf ppf "_%d [label=\"%a\";shape=circle];\n" vertex
          IrPrettyPrinter.pp_register vertex;
      Hashtbl.add vertices vertex true)
  in

  let pp_intf_edge ppf u v =
    if not (Hashtbl.mem intfs_edges (u, v) || Hashtbl.mem intfs_edges (v, u))
    then (
      Format.fprintf ppf "_%d -- _%d;\n" u v;
      Hashtbl.add intfs_edges (u, v) true)
  in

  let pp_pref_edge ppf u v =
    if not (Hashtbl.mem prefs_edges (u, v) || Hashtbl.mem prefs_edges (v, u))
    then (
      Format.fprintf ppf "_%d -- _%d [style=dashed];\n" u v;
      Hashtbl.add prefs_edges (u, v) true)
  in

  let pp_graph ppf graph =
    Reg.Map.iter
      (fun r1 arc ->
        pp_vertex ppf r1;

        Reg.Set.iter
          (fun r2 ->
            pp_vertex ppf r2;
            pp_intf_edge ppf r1 r2)
          arc.intfs;

        Reg.Set.iter
          (fun r2 ->
            pp_vertex ppf r2;
            pp_pref_edge ppf r1 r2)
          arc.prefs)
      graph
  in

  Format.printf "graph Interference {\nlayout=circo;\n%a}@." pp_graph graph
