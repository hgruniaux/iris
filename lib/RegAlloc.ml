(*
  This file contains the implementation of the register allocation algorithm.
  We use the George and Appel algorithm (Iterated Reg Coalescing, 1996).
*)

open Mir
open Interference

(* Some module alias for easier typing. *)
module M = Reg.Map
module S = Reg.Set

type color = Reg of reg | Spilled of int (* frame index *)
type coloring = color M.t

exception Found of reg
exception Found2 of reg * reg

(** Given a list of couples (value, cost), returns the value with the lowest cost.
    The list must be non empty. *)
let min_of_candidates l =
  fst
    (List.fold_left
       (fun (v, k) (min_v, min_k) ->
         if k < min_k then (v, k) else (min_v, min_k))
       (List.hd l) l)

(** Finds a vertex of degree < [k], without any preference edges
    (if [allow_prefs] is set to false), in [g].
    If such vertex does not exist, [Not_found] is raised. *)
let find_minimal_deg_vertex k g ~allow_prefs =
  let candidates =
    M.fold
      (fun v a l ->
        let deg = S.cardinal a.intfs in
        if
          Reg.is_physical v (* do not touch pre-colored vertices *)
          && deg < k
          && (allow_prefs || S.is_empty a.prefs)
        then (v, deg) :: l
        else l)
      g []
  in
  if candidates = [] then raise Not_found else min_of_candidates candidates

(** Chooses a register in [g] that will be spilled. Ideally, we want to choose
    a register that minimize a given cost (the spilling cost). *)
let choose_spill_reg g =
  (* We avoid to select a register that was used to store the temporary
     result of a spilled register's load. *)
  fst (M.choose g)

(** Returns a new interference graph based on [g] but without the
    preferences edges connected to [v]. *)
let forget_prefs_of v g =
  match M.find_opt v g with
  | None -> g (* nothing to forget *)
  | Some a ->
      (* Updates edges. *)
      let g =
        S.fold
          (fun w g ->
            let a = M.find w g in
            M.add w { intfs = a.intfs; prefs = S.remove v a.prefs } g)
          a.prefs g
      in
      (* Removes preferences of v. *)
      M.add v { intfs = a.intfs; prefs = S.empty } g

(** Try to find an edge that verify the George criteria for register coalescing.
    If no such edge is found, [Not_found] is raised. *)
let find_george_edge k g =
  try
    M.iter
      (fun v1 a ->
        (* Briggs criteria: safe to coalesce x and y if the resulting node
           will have fewer than k neighbors with degree >= k. *)
        S.iter
          (fun v2 ->
            if v2 <> v1 then
              let a2 = M.find v2 g in
              let merged_intfs = S.union a.intfs a2.intfs in
              let with_deg_over_k =
                S.filter
                  (fun v ->
                    let a = M.find v g in
                    S.cardinal a.intfs >= k)
                  merged_intfs
              in
              if S.cardinal with_deg_over_k < k then raise (Found2 (v1, v2)))
          a.prefs;

        (* George criteria: *)
        S.iter
          (fun v2 ->
            if v2 <> v1 then
              let a2 = M.find v2 g in
              let is_candidate =
                S.for_all
                  (fun w ->
                    let aw = M.find w g in
                    S.cardinal aw.intfs < k || S.mem v1 aw.intfs)
                  a2.intfs
              in
              if is_candidate then raise (Found2 (v1, v2)))
          a.prefs)
      g;

    (*
        (* Neighbours with degree >= K. *)
        let w =
          S.filter
            (fun r ->
              let a = M.find r g in
              S.cardinal a.prefs >= k)
            a.prefs
        in
        (* Neighbours that are physical registers. *)
        let p = S.filter (fun r -> Reg.is_physical r) a.prefs in
        (* Neighbours that are pseudo registers. *)
        let s = S.filter (fun r -> Reg.is_pseudo r) a.prefs in

        let c1 = S.union w p in
        let c2 = S.union w s in

        let v2 =
          S.choose_opt
            (S.filter
               (fun v2 ->
                 v2 <> v1
                 &&
                 let a = M.find v2 g in
                 S.subset c1 a.prefs)
               s)
        in
        match v2 with
        | None -> (
            let v2 =
              S.choose_opt
                (S.filter
                   (fun v2 ->
                     v2 <> v1
                     &&
                     let a = M.find v2 g in
                     S.subset c2 a.prefs)
                   p)
            in
            match v2 with None -> () | Some v2 -> raise (Found2 (v1, v2)))
        | Some v2 -> raise (Found2 (v1, v2)))
      g;*)
    (* We failed, there is no such edge. *)
    raise Not_found
  with Found2 (v1, v2) -> (v1, v2)

(** Merge the vertices [v1] and [v2] of the graph [g]. The returned graph
    does not contain anymore the vertex [v1] but the vertex [v2] now
    has all the previous edges of [v1] plus the previous edges of [v2] (they are merged). *)
let merge g v1 v2 =
  let replace s v1 v2 = if S.mem v1 s then S.add v2 (S.remove v1 s) else s in

  let a1 = M.find v1 g in
  let a2 = M.find v2 g in
  let g =
    S.fold
      (fun w g ->
        let a = M.find w g in
        let new_a =
          { intfs = replace a.intfs v1 v2; prefs = replace a.prefs v1 v2 }
        in
        M.add w new_a g)
      (S.union a1.intfs a1.prefs)
      g
  in
  M.add v2
    { intfs = S.union a1.intfs a2.intfs; prefs = S.union a1.prefs a2.prefs }
    (M.remove v1 g)

(** Given a set of all available colors [all_colors], an actual partial [coloring],
    a not colored vertex [v] in a given interference graph [g], returns a possible
    color that can be attached to [v]. If [v] is a physical register,
    then it is precolored and therefore [v] is returned itself.

    If no possible color is found, then [Not_found] is raised. *)
let find_available_color all_colors coloring v g =
  (* Physical registers are already precolored by definition. *)
  if Reg.is_physical v then v
  else
    match M.find_opt v g with
    | None ->
        (* If there is no interference for v, choose any color. *)
        S.choose all_colors
    | Some a ->
        (* Otherwise, choose a possible color. The set of possible colors
           are the one not taken by other registers which interfers with v. *)
        let possible =
          S.fold
            (fun w possible ->
              match M.find_opt w coloring with
              | Some (Reg c) -> S.remove c possible
              | _ -> possible)
            a.intfs all_colors
        in

        S.choose possible

(*
 * The graph coloration algorithm:
 *   - simplify: try to find a vertex with deg < k to color it
 *   - coalesce: try to coalesce two registers
 *   - freeze: removes the preferences edges of a vertex so we don't try to coalesce anymore
 *   - spill: choose a register to spill (if we failed to find a vertex to color)
 *   - select: color the given vertex with the first possible color or spill it
 * The algorithm used here is quite classic and many documentation can be found
 * on the Web about it. For reference, the algorithm is called Iterated register
 * coalescing and Chaitin's algorithm.
 *)

let rec simplify all_colors g =
  try
    (* Try to find a vertex with a degree < k (where k is the number of
       available colors). Such a vertex can be trivially colored. Otherwise,
       try to coalesce two registers. *)
    let k = S.cardinal all_colors in
    let v = find_minimal_deg_vertex k g ~allow_prefs:false in
    select all_colors g v
  with Not_found -> coalesce all_colors g

and coalesce all_colors g =
  try
    (* Try to find two registers (move-related) that can be coalesced.
       If we fail to find such vertex, then forget their preferences edges. *)
    let k = S.cardinal all_colors in
    let v1, v2 = find_george_edge k g in
    let g = merge g v1 v2 in
    let coloring = simplify all_colors g in
    M.add v1 (M.find v2 coloring) coloring
  with Not_found -> freeze all_colors g

and freeze all_colors g =
  try
    let k = S.cardinal all_colors in
    let v = find_minimal_deg_vertex k g ~allow_prefs:true in
    let g = forget_prefs_of v g in
    simplify all_colors g
  with Not_found -> spill all_colors g

and spill all_colors g =
  if M.is_empty g then M.empty (* empty coloring *)
  else
    let v = choose_spill_reg g in
    select all_colors g v

and select all_colors g v =
  let coloring = simplify all_colors (Interference.remove g v) in
  try
    let selected_color = find_available_color all_colors coloring v g in
    M.add v (Reg selected_color) coloring
  with Not_found -> M.add v (Spilled 0) coloring

let color arch g =
  let all_colors = Backend.registers arch in
  let coloring = simplify all_colors g in
  coloring

let dump_colors colors =
  Format.printf "Coloration (register allocation):@.";
  Reg.Map.iter
    (fun reg color ->
      if Reg.is_pseudo reg then
        match color with
        | Reg color ->
            Format.printf "%a -> %a@." IrPrettyPrinter.pp_register reg
              IrPrettyPrinter.pp_register color
        | Spilled n ->
            Format.printf "%a -> Spilled %d@." IrPrettyPrinter.pp_register reg n)
    colors
