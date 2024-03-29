open Format
open Lexing
open Lexer
open LibIris

let () = Printexc.record_backtrace true
let usage = "usage: violet [options] file.v"
let parse_only = ref false
let arch = ref Backend.Arch_x64

let set_arch arch_name =
  arch :=
    match arch_name with
    | "x86" -> Backend.Arch_x86
    | "x86-64" | "x64" -> Backend.Arch_x64
    | _ -> raise (Arg.Bad "unknown architecture name")

let spec =
  Arg.align
    ([
       ("--parse-only", Arg.Set parse_only, "\tstop after parsing");
       ( "--arch",
         Arg.Symbol ([ "x86"; "x86-64"; "x64"; "cpulm" ], set_arch),
         "\tselects the target architecture" );
     ]
    @ CmdArgs.spec)

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
