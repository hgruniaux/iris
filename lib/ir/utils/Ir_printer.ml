open Ir_base
open Format
module Value = Ir_value
module Function = Ir_function
module Block = Ir_block

type color =
  | Default
  | Comment
  | Keyword
  | Label
  | Type
  | Constant
  | Register
  | Global

module type Formatter = sig
  val pp_with_color :
    color -> (formatter -> 'a -> unit) -> formatter -> 'a -> unit
end

module BasicFormatter : Formatter = struct
  let pp_with_color _ pp = pp
end

module AnsiColorsFormatter : Formatter = struct
  let comment_color = "90;3" (* Dark gray and italic *)
  let keyword_color = "33" (* Yellow *)
  let label_color = "36;3" (* Cyan and italic *)
  let type_color = "35" (* Magenta *)
  let constant_color = "32" (* Green *)
  let register_color = "31;3" (* Red *)
  let global_color = "34;3" (* Blue and italic *)

  let code_for_color = function
    | Default -> "0"
    | Comment -> comment_color
    | Keyword -> keyword_color
    | Label -> label_color
    | Type -> type_color
    | Constant -> constant_color
    | Register -> register_color
    | Global -> global_color

  let pp_with_color color pp =
    let color_code = code_for_color color in
    fun fmt v ->
      fprintf fmt "\027[%sm%a" color_code pp v;
      fprintf fmt "\027[0m"
end

module Make (F : Formatter) = struct
  open F

  let pp_comment fmt comment =
    pp_with_color Comment (fun fmt c -> fprintf fmt "@[// %s@]@," c) fmt comment

  let pp_keyword = pp_with_color Keyword pp_print_string
  let pp_register fmt r = pp_with_color Register Reg.pp_print fmt r
  let pp_bool fmt b = pp_with_color Constant pp_print_bool fmt b
  let pp_integer fmt n = pp_with_color Constant Z.pp_print fmt n
  let pp_float fmt f = pp_with_color Constant pp_print_float fmt f

  let pp_string fmt s =
    pp_with_color Constant
      (fun fmt str -> fprintf fmt "\"%s\"" (String.escaped str))
      fmt s

  let pp_label = pp_with_color Label Label.pp_print

  let pp_global_ref =
    let unnamed_globals = Hashtbl.create 17 in
    let cpt = ref 0 in
    let pp out global =
      match global.global_name with
      | Some name -> fprintf out "@%s" name
      | None -> (
          match Hashtbl.find_opt unnamed_globals global with
          | None ->
              incr cpt;
              Hashtbl.add unnamed_globals global !cpt;
              fprintf out "@_unnamed_%d" !cpt
          | Some i -> fprintf out "@_unnamed_%d" i)
    in
    pp_with_color Global pp

  let pp_function_ref =
    let pp fmt fn = fprintf fmt "@%s" fn.fn_name in
    pp_with_color Global pp

  let rec pp_type fmt = function
    | Ityp_unit -> pp_keyword fmt "unit"
    | Ityp_i1 -> pp_keyword fmt "i1"
    | Ityp_i8 -> pp_keyword fmt "i8"
    | Ityp_i16 -> pp_keyword fmt "i16"
    | Ityp_i32 -> pp_keyword fmt "i32"
    | Ityp_i64 -> pp_keyword fmt "i64"
    | Ityp_f32 -> pp_keyword fmt "f32"
    | Ityp_f64 -> pp_keyword fmt "f64"
    | Ityp_ptr -> pp_keyword fmt "ptr"
    | Ityp_array (elem_ty, size) ->
        fprintf fmt "[%a x %a]" pp_type elem_ty
          (pp_with_color Constant pp_print_int)
          size
    | Ityp_func (params_types, return_type, is_variadic) ->
        fprintf fmt "%a (%a%a) -> %a" pp_keyword "fn"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_type)
          params_types
          (fun fmt () -> if is_variadic then fprintf fmt ", ..." else ())
          () pp_type return_type
    | Ityp_struct field_types ->
        fprintf fmt "{ %a }"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_type)
          field_types

  let pp_value fmt = function
    | Ival_reg r -> pp_register fmt r
    | Ival_int (typ, n) -> fprintf fmt "(%a %a)" pp_type typ pp_integer n
    | Ival_float (typ, n) -> fprintf fmt "(%a %a)" pp_type typ pp_float n
    | Ival_global g -> pp_global_ref fmt g

  let pp_ibinop =
    let pp fmt = function
      | Ibinop_add -> fprintf fmt "add"
      | Ibinop_sub -> fprintf fmt "sub"
      | Ibinop_mul -> fprintf fmt "mul"
      | Ibinop_div_u -> fprintf fmt "div_u"
      | Ibinop_div_s -> fprintf fmt "div_s"
      | Ibinop_rem_u -> fprintf fmt "rem_u"
      | Ibinop_rem_s -> fprintf fmt "rem_s"
      | Ibinop_lsl -> fprintf fmt "lsl"
      | Ibinop_asr -> fprintf fmt "asr"
      | Ibinop_lsr -> fprintf fmt "lsr"
      | Ibinop_and -> fprintf fmt "and"
      | Ibinop_or -> fprintf fmt "or"
      | Ibinop_xor -> fprintf fmt "xor"
    in
    pp_with_color Keyword pp

  let pp_iunop =
    let pp fmt = function
      | Iunop_neg -> fprintf fmt "neg"
      | Iunop_not -> fprintf fmt "not"
    in
    pp_with_color Keyword pp

  let pp_icmp =
    let pp fmt = function
      | Icmp_eq -> fprintf fmt "eq"
      | Icmp_ne -> fprintf fmt "ne"
      | Icmp_lt_u -> fprintf fmt "lt_u"
      | Icmp_le_u -> fprintf fmt "le_u"
      | Icmp_gt_u -> fprintf fmt "gt_u"
      | Icmp_ge_u -> fprintf fmt "ge_u"
      | Icmp_lt_s -> fprintf fmt "lt_s"
      | Icmp_le_s -> fprintf fmt "le_s"
      | Icmp_gt_s -> fprintf fmt "gt_s"
      | Icmp_ge_s -> fprintf fmt "ge_s"
    in
    pp_with_color Keyword pp

  let pp_castop =
    let pp fmt = function
      | Icast_extend_s -> fprintf fmt "extend_s"
      | Icast_extend_u -> fprintf fmt "extend_u"
      | Icast_trunc -> fprintf fmt "trunc"
      | Icast_promote -> fprintf fmt "promote"
      | Icast_demote -> fprintf fmt "demote"
      | Icast_fp2ui -> fprintf fmt "fp2ui"
      | Icast_fp2si -> fprintf fmt "fp2si"
      | Icast_ui2fp -> fprintf fmt "ui2fp"
      | Icast_si2fp -> fprintf fmt "si2fp"
      | Icast_ptr2int -> fprintf fmt "ptr2int"
      | Icast_int2ptr -> fprintf fmt "int2ptr"
      | Icast_bitcast -> fprintf fmt "bitcast"
    in
    pp_with_color Keyword pp

  let pp_list pp = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp

  let pp_regset fmt s =
    let l = RegSet.elements s in
    pp_list pp_register fmt l

  let pp_labelset fmt s =
    let l = LabelSet.elements s in
    pp_list pp_label fmt l

  let pp_expression fmt expression =
    match expression with
    | Iexpr_value v -> fprintf fmt "%a" pp_value v
    | Iexpr_alloca (t, align) ->
        fprintf fmt "%a %a, %a %a" pp_keyword "alloca" pp_type t pp_keyword
          "align"
          (pp_with_color Constant pp_print_int)
          align
    | Iexpr_load (t, r) ->
        fprintf fmt "%a.%a %a" pp_type t pp_keyword "load" pp_value r
    | Iexpr_ibinop (binop, v1, v2) ->
        fprintf fmt "%a.%a %a, %a" pp_type (Value.type_of v1) pp_ibinop binop
          pp_value v1 pp_value v2
    | Iexpr_iunop (unop, v) ->
        fprintf fmt "%a.%a %a" pp_type (Value.type_of v) pp_iunop unop pp_value
          v
    | Iexpr_icmp (cmp, v1, v2) ->
        fprintf fmt "%a.%a %a, %a" pp_type (Value.type_of v1) pp_icmp cmp
          pp_value v1 pp_value v2
    | Iexpr_cast (Icast_bitcast, t, v) ->
        fprintf fmt "%a %a %a %a" pp_castop Icast_bitcast pp_value v pp_keyword
          "to" pp_type t
    | Iexpr_cast (castop, t, v) ->
        fprintf fmt "%a.%a %a" pp_type t pp_castop castop pp_value v
    | Iexpr_call (callee, args) ->
        fprintf fmt "%a %a(@[%a@])" pp_keyword "call" pp_value callee
          (pp_list pp_value) args

  let pp_instruction fmt instruction =
    match instruction with
    | Iinst_def (n, expr) ->
        fprintf fmt "%a = %a" pp_register n pp_expression expr
    | Iinst_store (addr, value) ->
        fprintf fmt "%a %a, %a" pp_keyword "store" pp_value addr pp_value value

  let pp_label_with_args fmt (label, args) =
    match args with
    | [] -> pp_label fmt label
    | _ -> fprintf fmt "%a(%a)" pp_label label (pp_list pp_value) args

  let pp_label_with_params fmt (label, params) =
    match params with
    | [] -> pp_label fmt label
    | _ ->
        fprintf fmt "%a(%a)" pp_label label
          (pp_list (fun fmt reg ->
               fprintf fmt "%a %a" pp_type (Reg.type_of reg) pp_register reg))
          params

  let pp_terminator fmt term =
    match term with
    | Iterm_unreachable -> pp_keyword fmt "unreachable"
    | Iterm_ret None -> pp_keyword fmt "ret"
    | Iterm_ret (Some v) -> fprintf fmt "%a %a" pp_keyword "ret" pp_value v
    | Iterm_br (target_label, target_args) ->
        fprintf fmt "%a %a" pp_keyword "br" pp_label_with_args
          (target_label, target_args)
    | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
        fprintf fmt "%a %a, %a, %a" pp_keyword "br_if" pp_value cond
          pp_label_with_args (true_label, true_args) pp_label_with_args
          (false_label, false_args)
    | Iterm_br_table (index, default_label, default_args, cases) ->
        fprintf fmt "%a %a, %a, [@[%a@]]" pp_keyword "br_table" pp_value index
          pp_label_with_args
          (default_label, default_args)
          (pp_list (fun fmt (value, label, args) ->
               fprintf fmt "@[(%a,@ %a)@]" Z.pp_print value pp_label_with_args
                 (label, args)))
          cases

  let pp_basic_block fmt bb =
    fprintf fmt "@[<v2>%a:@," pp_label_with_params
      (Block.label bb, Block.params bb);

    List.iter
      (fun inst -> fprintf fmt "%a@," pp_instruction inst)
      bb.block_insts;

    fprintf fmt "%a@]@," pp_terminator (Block.term bb)

  let rec pp_params fmt (regs, types) =
    match (regs, types) with
    | [], [] -> ()
    | [ reg ], [ typ ] -> fprintf fmt "%a %a" pp_type typ pp_register reg
    | reg :: l, typ :: l' ->
        fprintf fmt "%a %a, %a" pp_type typ pp_register reg pp_params (l, l')
    | _ -> assert false

  let pp_function fmt fn =
    let return_type, param_types, is_variadic =
      Function.return_and_params_types_of fn
    in

    if fn.fn_is_external then
      fprintf fmt "@[%a %a(@[%a%a@]) -> %a;@]" pp_keyword "extern fn"
        pp_function_ref fn pp_params
        (fn.fn_params, param_types)
        (fun fmt () -> if is_variadic then fprintf fmt ", ..." else ())
        () pp_type return_type
    else (
      fprintf fmt "@[<v>%a %a(%a%a) -> %a {@," pp_keyword "fn" pp_function_ref
        fn pp_params
        (fn.fn_params, param_types)
        (fun fmt () -> if is_variadic then fprintf fmt ", ..." else ())
        () pp_type return_type;
      LabelMap.iter (fun _ b -> fprintf fmt "%a" pp_basic_block b) fn.fn_blocks;
      fprintf fmt "}@]")

  let pp_constant_value fmt constant =
    match constant with
    | Iconstant_uninitialized -> pp_keyword fmt "uninitialized"
    | Iconstant_bytes bytes ->
        pp_with_color Constant
          (fun fmt b -> fprintf fmt "\"%a\"" pp_print_bytes (Bytes.escaped b))
          fmt bytes
    | Iconstant_int n -> pp_integer fmt n
    | Iconstant_float f -> pp_float fmt f

  let pp_global_variable fmt global var_type init_value =
    fprintf fmt "@[%a %a : %a = %a;@]" pp_keyword "global" pp_global_ref global
      pp_type var_type pp_constant_value init_value

  let pp_global fmt global =
    match global.global_kind with
    | Iglobal_variable (var_type, init_value) ->
        pp_global_variable fmt global var_type init_value
    | Iglobal_function fn -> pp_function fmt fn
end

module Printer = Make (BasicFormatter)
module ColorPrinter = Make (AnsiColorsFormatter)
include ColorPrinter

let dump_ir fn =
  let fmt = std_formatter in
  pp_function fmt fn

let dump_module ctx =
  let fmt = std_formatter in

  fprintf fmt "@[<v>";

  let extern_funcs, funcs, unnamed_globals, named_globals =
    List.fold_left
      (fun (extern_funcs, funcs, unnamed_globals, named_globals) global ->
        match global.global_kind with
        | Iglobal_function fn ->
            if fn.fn_is_external then
              (global :: extern_funcs, funcs, unnamed_globals, named_globals)
            else (extern_funcs, global :: funcs, unnamed_globals, named_globals)
        | Iglobal_variable _ ->
            if global.global_name = None then
              (extern_funcs, funcs, global :: unnamed_globals, named_globals)
            else (extern_funcs, funcs, unnamed_globals, global :: named_globals))
      ([], [], [], []) ctx.mod_globals
  in

  let print_globals ?(newlines = false) globals =
    if globals <> [] then (
      List.iter
        (fun global ->
          pp_global fmt global;
          fprintf fmt "@,";
          if newlines then fprintf fmt "@,")
        globals;
      fprintf fmt "@,")
  in

  print_globals unnamed_globals;
  print_globals named_globals;
  print_globals extern_funcs;
  print_globals ~newlines:true funcs;

  fprintf fmt "@]@."
