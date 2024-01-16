open Mr
open PPrintIr

let pp_operand ppf op =
  match op with
  | Oreg r -> pp_register ppf r
  | Oframe n -> Format.fprintf ppf "STACK[%d]" n
  | Oimm i -> Z.pp_print ppf i
  | Oconst c -> Format.fprintf ppf "%a" pp_constant c
  | Olabel l -> Format.fprintf ppf "%a" pp_label l
  | Ofunc fn -> Format.fprintf ppf "%s" fn.fn_name

let pp_inst ppf inst =
  Format.fprintf ppf "%s %a" inst.mi_kind (pp_list pp_operand) inst.mi_operands

let pp_bb pp_extra_bb pp_extra_inst ppf bb =
  Format.fprintf ppf "%a: %a@." pp_label bb.mbb_label pp_extra_bb bb;
  List.iter
    (fun inst -> Format.fprintf ppf "  %a %a@." pp_inst inst pp_extra_inst inst)
    bb.mbb_insts

let pp_preds_and_succs ppf bb =
  if not (Label.Set.is_empty bb.mbb_predecessors) then
    Format.fprintf ppf "; preds = %a" pp_labelset bb.mbb_predecessors

let pp_defs_and_uses ppf inst =
  Format.fprintf ppf "; defs = {%a}, uses = {%a}" pp_registerset inst.mi_defs
    pp_registerset inst.mi_uses

let pp_fn pp_extra_bb pp_extra_inst ppf fn =
  Format.fprintf ppf "fn %s() {\n" fn.mfn_name;
  Label.Map.iter
    (fun _ bb -> pp_bb pp_extra_bb pp_extra_inst ppf bb)
    fn.mfn_blocks;
  Format.fprintf ppf "}"

let pp ppf fns =
  List.iter (fun fn -> pp_fn (fun _ _ -> ()) (fun _ _ -> ()) ppf fn) fns

let pp_extra ppf fns =
  List.iter (fun fn -> pp_fn pp_preds_and_succs pp_defs_and_uses ppf fn) fns

let dump_mir fns = Format.printf "%a@." pp_extra fns
