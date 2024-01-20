open Ir

let pp_register =
  let regs = Hashtbl.create 17 in
  let cpt = ref 0 in
  fun out reg ->
    if not (Reg.is_pseudo reg) then Format.fprintf out "r%d" reg
    else
      match Hashtbl.find_opt regs reg with
      | None ->
          incr cpt;
          Hashtbl.add regs reg !cpt;
          Format.fprintf out "%%%d" !cpt
      | Some i -> Format.fprintf out "%%%d" i

let pp_label =
  let labels = Hashtbl.create 17 in
  let cpt = ref 0 in
  fun out label ->
    match Hashtbl.find_opt labels label with
    | None ->
        incr cpt;
        Hashtbl.add labels label !cpt;
        Format.fprintf out "L%d" !cpt
    | Some i -> Format.fprintf out "L%d" i

let rec pp_list pp ppf = function
  | [] -> ()
  | [ hd ] -> Format.fprintf ppf "%a" pp hd
  | hd :: tl ->
      Format.fprintf ppf "%a, " pp hd;
      pp_list pp ppf tl

let pp_registerset ppf s =
  let l = Reg.Set.elements s in
  pp_list pp_register ppf l

let pp_labelset ppf s =
  let l = Label.Set.elements s in
  pp_list pp_label ppf l

let rec pp_type ppf = function
  | Ityp_void -> Format.fprintf ppf "void"
  | Ityp_int -> Format.fprintf ppf "int"
  | Ityp_ptr -> Format.fprintf ppf "ptr"
  | Ityp_func (ret, args) ->
      Format.fprintf ppf "%a (%a)" pp_type ret (pp_list pp_type) args
  | Ityp_struct fields -> Format.fprintf ppf "{ %a }" (pp_list pp_type) fields

let pp_binop ppf = function
  | Ibinop_add -> Format.fprintf ppf "add"
  | Ibinop_sub -> Format.fprintf ppf "sub"
  | Ibinop_mul -> Format.fprintf ppf "mul"
  | Ibinop_udiv -> Format.fprintf ppf "udiv"
  | Ibinop_sdiv -> Format.fprintf ppf "sdiv"
  | Ibinop_urem -> Format.fprintf ppf "urem"
  | Ibinop_srem -> Format.fprintf ppf "srem"
  | Ibinop_lsl -> Format.fprintf ppf "lsl"
  | Ibinop_asr -> Format.fprintf ppf "asr"
  | Ibinop_lsr -> Format.fprintf ppf "lsr"
  | Ibinop_and -> Format.fprintf ppf "and"
  | Ibinop_or -> Format.fprintf ppf "or"
  | Ibinop_xor -> Format.fprintf ppf "xor"

let pp_unop ppf = function
  | Iunop_neg -> Format.fprintf ppf "neg"
  | Iunop_not -> Format.fprintf ppf "not"

let pp_cmp ppf = function
  | Icmp_eq -> Format.fprintf ppf "eq"
  | Icmp_ne -> Format.fprintf ppf "ne"
  | Icmp_ult -> Format.fprintf ppf "ult"
  | Icmp_ule -> Format.fprintf ppf "ule"
  | Icmp_ugt -> Format.fprintf ppf "ugt"
  | Icmp_uge -> Format.fprintf ppf "uge"
  | Icmp_slt -> Format.fprintf ppf "slt"
  | Icmp_sle -> Format.fprintf ppf "sle"
  | Icmp_sgt -> Format.fprintf ppf "sgt"
  | Icmp_sge -> Format.fprintf ppf "sge"

let rec pp_constant_value ppf constant =
  match constant with
  | Icst_int n -> Z.pp_print ppf n
  | Icst_string str -> Format.fprintf ppf "\"%s\"" (String.escaped str)
  | Icst_function fn -> Format.fprintf ppf "@%s" fn.fn_name
  | Icst_struct fields ->
      Format.fprintf ppf "{ %a }" (pp_list pp_constant_value) fields

let pp_constant =
  let constants = Hashtbl.create 17 in
  let cpt = ref 0 in
  fun ppf constant ->
    match Hashtbl.find_opt constants constant with
    | None ->
        incr cpt;
        Hashtbl.add constants constant !cpt;
        Format.fprintf ppf "LC%d" !cpt
    | Some i -> Format.fprintf ppf "LC%d" i

let pp_operand ppf operand =
  match operand with
  | Iop_reg reg -> pp_register ppf reg
  | Iop_imm imm -> Z.pp_print ppf imm

let pp_instruction ppf inst =
  let n = inst.i_name in
  match inst.i_kind with
  | Iinst_cst cst -> Format.fprintf ppf "%a = %a" pp_register n pp_constant cst
  | Iinst_loadi cst -> Format.fprintf ppf "%a = %a" pp_register n Z.pp_print cst
  | Iinst_mov r -> Format.fprintf ppf "%a = %a" pp_register n pp_register r
  | Iinst_load (t, r) ->
      Format.fprintf ppf "%a = load %a %a" pp_register n pp_type t pp_register r
  | Iinst_loadfield (t, r, i) ->
      Format.fprintf ppf "%a = loadfield %a %a %d" pp_register n pp_type t
        pp_register r i
  | Iinst_store (r, op) ->
      Format.fprintf ppf "store %a %a" pp_register r pp_operand op
  | Iinst_binop (binop, op1, op2) ->
      Format.fprintf ppf "%a = %a %a %a" pp_register n pp_binop binop pp_operand
        op1 pp_operand op2
  | Iinst_unop (unop, op) ->
      Format.fprintf ppf "%a = %a %a" pp_register n pp_unop unop pp_operand op
  | Iinst_cmp (cmp, op1, op2) ->
      Format.fprintf ppf "%a = %a %a %a" pp_register n pp_cmp cmp pp_operand op1
        pp_operand op2
  | Iinst_call (fn, args) ->
      Format.fprintf ppf "%a = call %s(%a)" pp_register n fn.fn_name
        (pp_list pp_operand) args
  | Iinst_phi predecessors ->
      Format.fprintf ppf "%a = phi [%a]" pp_register n
        (pp_list (fun ppf (op, label) ->
             Format.fprintf ppf "(%a, %a)" pp_operand op pp_label label))
        predecessors

let pp_terminator ppf term =
  match term with
  | Iterm_unreachable -> Format.fprintf ppf "unreachable"
  | Iterm_ret -> Format.fprintf ppf "ret"
  | Iterm_retv op -> Format.fprintf ppf "ret %a" pp_operand op
  | Iterm_jmp bb -> Format.fprintf ppf "jmp %a" pp_label bb
  | Iterm_jmpc (op, then_bb, else_bb) ->
      Format.fprintf ppf "jmpc %a %a %a" pp_operand op pp_label then_bb pp_label
        else_bb
  | Iterm_switch (index, otherwise, targets) ->
      Format.fprintf ppf "switch %a %a [%a]" pp_operand index pp_label otherwise
        (pp_list (fun ppf (value, label) ->
             Format.fprintf ppf "(%a, %a)" Z.pp_print value pp_label label))
        targets

let pp_bb ppf bb =
  Format.fprintf ppf "%a:" pp_label bb.b_label;
  if not (Label.Set.is_empty bb.b_predecessors) then
    Format.fprintf ppf " ; preds = %a" pp_labelset bb.b_predecessors;
  Format.fprintf ppf "@.";

  List.iter
    (fun inst -> Format.fprintf ppf "  %a@." pp_instruction inst)
    bb.b_phi_insts;
  List.iter
    (fun inst -> Format.fprintf ppf "  %a@." pp_instruction inst)
    bb.b_insts;

  Format.fprintf ppf "  %a@." pp_terminator bb.b_term

let rec pp_params ppf (regs, types) =
  match (regs, types) with
  | [], [] -> ()
  | [ reg ], [ typ ] -> Format.fprintf ppf "%a %a" pp_type typ pp_register reg
  | reg :: l, typ :: l' ->
      Format.fprintf ppf "%a %a, %a" pp_type typ pp_register reg pp_params
        (l, l')
  | _ -> assert false

let pp_fn ppf fn =
  if fn.fn_is_external then
    Format.fprintf ppf "extern fn %s(%a) -> %a;@." fn.fn_name pp_params
      (fn.fn_params, param_types_of fn)
      pp_type (return_type_of fn)
  else (
    Format.fprintf ppf "fn %s(%a) -> %a {@." fn.fn_name pp_params
      (fn.fn_params, param_types_of fn)
      pp_type (return_type_of fn);
    Label.Map.iter (fun _ b -> Format.fprintf ppf "%a" pp_bb b) fn.fn_blocks;
    Format.fprintf ppf "}@.")

let dump_ir fn =
  let ppf = Format.std_formatter in
  pp_fn ppf fn

let dump_ctx ctx =
  let ppf = Format.std_formatter in

  Hashtbl.iter
    (fun name cst_value ->
      Format.fprintf ppf "%a = constant %a\n" pp_constant name pp_constant_value
        cst_value)
    ctx.ctx_constants;
  List.iter (pp_fn ppf) ctx.ctx_funcs

let dump_dot funcdef =
  let pp_bb_in_dot ppf bb =
    Format.fprintf ppf "%a:\\l" pp_label bb.b_label;

    List.iter
      (fun inst -> Format.fprintf ppf "    %a\\l" pp_instruction inst)
      bb.b_phi_insts;
    List.iter
      (fun inst -> Format.fprintf ppf "    %a\\l" pp_instruction inst)
      bb.b_insts;

    Format.fprintf ppf "    %a\\l" pp_terminator bb.b_term
  in

  Format.printf "digraph %s {@." funcdef.fn_name;
  Label.Map.iter
    (fun label bb ->
      Format.printf "%a [shape=rect; label=\"@[%a@]\"]@." pp_label label
        pp_bb_in_dot bb;

      Label.Set.iter
        (fun target ->
          Format.printf "%a -> %a;@." pp_label label pp_label target)
        bb.b_successors)
    funcdef.fn_blocks;
  Format.printf "}@."
