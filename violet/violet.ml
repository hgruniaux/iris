open Format
open Lexing
open Lexer
open LibIris

let () = Printexc.record_backtrace true
let usage = "usage: violet [options] file.v"
let parse_only = ref false
let arch = ref Backend.Arch_x64
let dump_opt_ir = ref false
let dump_opt_ir_dot = ref false

let set_arch arch_name =
  arch :=
    match arch_name with
    | "x86" -> Backend.Arch_x86
    | "x86-64" | "x64" -> Backend.Arch_x64
    | "cpulm" -> Backend.Arch_cpulm
    | _ -> raise (Arg.Bad "unknown architecture name")

let spec =
  Arg.align
    [
      ("--parse-only", Arg.Set parse_only, "\tstop after parsing");
      ( "--arch",
        Arg.Symbol ([ "x86"; "x86-64"; "x64"; "cpulm" ], set_arch),
        "\tselects the target architecture" );
      ("--dump-ir", Arg.Set PassManager.dump_ir, "\tdumps generated IR");
      ( "--dump-ir-dot",
        Arg.Set PassManager.dump_ir_dot,
        "\tdumps generated IR in Graphviz DOT format" );
      ( "--dump-opt-ir",
        Arg.Set dump_opt_ir,
        "\tdumps generated IR after optimizations" );
      ( "--dump-opt-ir-dot",
        Arg.Set dump_opt_ir_dot,
        "\tdumps generated IR after optimizations in Graphviz DOT format" );
      ( "--dump-mir",
        Arg.Set PassManager.dump_mir,
        "\tdumps generated machine MIR" );
      ( "--dump-liveinfo",
        Arg.Set PassManager.dump_liveinfo,
        "\tdumps the liveness analysis result" );
      ( "--dump-interf",
        Arg.Set PassManager.dump_interf,
        "\tdumps the computed interference graph in Graphviz DOT format" );
      ( "--dump-reg-alloc",
        Arg.Set PassManager.dump_reg_alloc,
        "\tdumps the register allocation result" );
    ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".v") then raise (Arg.Bad "no .v extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let report (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc
(*
let compile_fn arch ir_fn =
  let pm = PassManager.create arch in
  let mir_fn = PassManager.run_on_fn pm ir_fn in
  mir_fn *)

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.program Lexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;

    let ctx = Ir.mk_ctx () in
    let ib = IrBuilder.create ctx in
    let _ = List.map (Compile.compile_func ib) f in
    let pm = PassManager.create !arch in
    PassManager.run_on_ctx pm Stdlib.stdout ctx
    (* let compiled_funcs = List.map (fun fn -> compile_fn !arch fn) ir_funcs in
       Backend.emit_ctx !arch Format.std_formatter ctx compiled_funcs *)
  with
  | Lexing_error msg ->
      let range_start = Lexing.lexeme_start_p lb in
      let range_end = Lexing.lexeme_end_p lb in
      report (range_start, range_end);
      eprintf "Error: %s@." msg
  | Parser.Error ->
      let range_start = Lexing.lexeme_start_p lb in
      let range_end = Lexing.lexeme_end_p lb in
      report (range_start, range_end);
      eprintf "Error: Syntax error.@."
