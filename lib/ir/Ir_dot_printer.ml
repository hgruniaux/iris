open Ir
open Format
module IrPrinter = Ir.Printer.Printer

let pp_label_ref fn fmt label =
  fprintf fmt "\"%s_cfg_%a\"" fn.fn_name IrPrinter.pp_label label

let pp_function_cluster_ref ?(dump_func_cfgs = true) fmt fn =
  if fn.fn_is_external then fprintf fmt "\"%s\"" fn.fn_name
  else if dump_func_cfgs && not (LabelMap.is_empty fn.fn_blocks) then
    fprintf fmt "cluster_cfg_%s" fn.fn_name
  else fprintf fmt "\"%s\"" fn.fn_name

let pp_function_ref ?(dump_func_cfgs = true) fmt fn =
  if fn.fn_is_external then fprintf fmt "\"%s\"" fn.fn_name
  else if dump_func_cfgs && not (LabelMap.is_empty fn.fn_blocks) then
    pp_label_ref fn fmt (Option.get fn.fn_entry)
  else fprintf fmt "%S" fn.fn_name

let dump_function_cfg fmt fn =
  fprintf fmt "subgraph %a@,@[<v2>{@,"
    (pp_function_cluster_ref ~dump_func_cfgs:true)
    fn;
  fprintf fmt "label = \"CFG of %s\";@," fn.fn_name;

  Cfg.iter_vertex
    (fun bb ->
      let code = asprintf "%a" IrPrinter.pp_basic_block bb in
      let escaped_code = String.escaped code in
      let left_justified =
        Str.global_replace (Str.regexp "\\\\n") "\\l" escaped_code
      in
      fprintf fmt "%a [label=\"%s\" shape=box]" (pp_label_ref fn) (Block.label bb)
        left_justified)
    fn;

  Cfg.iter_edges
    (fun (bb1, bb2) ->
      fprintf fmt "%a -> %a@," (pp_label_ref fn) (Block.label bb1) (pp_label_ref fn)
        (Block.label bb2))
    fn;

  fprintf fmt "@]@,}"

let dump_function ?(dump_func_cfgs = true) fmt fn =
  if fn.fn_is_external then
    fprintf fmt "%a [shape=box style=dashed]"
      (pp_function_ref ~dump_func_cfgs)
      fn
  else if dump_func_cfgs && not (LabelMap.is_empty fn.fn_blocks) then
    dump_function_cfg fmt fn
  else fprintf fmt "%a [shape=box]" (pp_function_ref ~dump_func_cfgs) fn

let dump_module ?(dump_func_cfgs = true) fmt m =
  let callgraph = CallGraph.compute m in
  fprintf fmt "@[<v>digraph IrModule@,@[<v2>{@,";
  fprintf fmt "label=\"Call graph of module\";@,";
  fprintf fmt "compound=true;@,";

  pp_print_list ~pp_sep:pp_print_cut
    (dump_function ~dump_func_cfgs)
    fmt callgraph.functions;
  fprintf fmt "@,";

  let edges = Hashtbl.create 7 in
  CallGraph.iter_edges
    (fun (fn1, fn2) ->
      let key = (fn1, fn2) in
      let count =
        match Hashtbl.find_opt edges key with None -> 0 | Some c -> c
      in
      Hashtbl.replace edges key (count + 1))
    callgraph;

  Hashtbl.iter
    (fun (fn1, fn2) count ->
      fprintf fmt "%a -> %a [label=\"%d call inst(s)\" ltail=%a lhead=%a]@,"
        (pp_function_ref ~dump_func_cfgs)
        fn1
        (pp_function_ref ~dump_func_cfgs)
        fn2 count
        (pp_function_cluster_ref ~dump_func_cfgs)
        fn1
        (pp_function_cluster_ref ~dump_func_cfgs)
        fn2)
    edges;

  fprintf fmt "@]@,}@]@."
