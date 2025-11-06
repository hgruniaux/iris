open LibIris

let () = Printexc.record_backtrace true
let usage = Format.asprintf "usage: %s [options] file.c" Sys.argv.(0)
let only_lexing = ref false
let only_parsing = ref false
let only_typing = ref false
let only_codegen = ref false
let dump_typed_ast = ref false
let dump_ir = ref false
let optimize = ref true
let output_filename = ref None

let spec =
  Arg.align
    [
      ("--only-lexing", Arg.Set only_lexing, "Stop after lexing");
      ("--only-parsing", Arg.Set only_parsing, "Stop after parsing");
      ("--only-typing", Arg.Set only_typing, "Stop after typing");
      ("--only-codegen", Arg.Set only_codegen, "Stop after IR generation");
      ("--dump-typed-ast", Arg.Set dump_typed_ast, "Dump the typed AST");
      ("--dump-ir", Arg.Set dump_ir, "Dump the generated IR");
      ("-O", Arg.Set optimize, "Enable optimizations (default)");
      ("-O1", Arg.Set optimize, "Enable optimizations (default)");
      ("-O0", Arg.Clear optimize, "Disable optimizations");
      ( "-o",
        Arg.String (fun s -> output_filename := Some s),
        "Specify output file" );
    ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let output_filename =
  match !output_filename with
  | Some f -> f
  | None -> Filename.remove_extension file ^ ".S"

let report_error ((b, e) : Ast.location) msg =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Format.eprintf "\x1b[1mFile \"%s\", line %d, characters %d-%d:\x1b[0m\n" file
    l fc lc;
  Format.eprintf "\x1b[1;31mError:\x1b[0m %s@." msg;
  exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let tu = Parser.translation_unit Lexer.lexer lb in
    close_in c;
    if !only_parsing then exit 0;

    let typed_tu = Typing.type_translation_unit tu in
    if !dump_typed_ast then
      Tast_printer.print_translation_unit Format.std_formatter typed_tu;
    if !only_typing then exit 0;

    let ir = Codegen.codegen_translation_unit typed_tu in
    if !dump_ir then Ir.Printer.dump_module ir;
    if !only_codegen then exit 0;

    let oc = open_out output_filename in
    let pm = PassManager.create Backend.Arch_x64 !optimize in
    PassManager.run_on_ctx pm oc ir
  with
  | Lexer.Error msg ->
      let range_start = Lexing.lexeme_start_p lb in
      let range_end = Lexing.lexeme_end_p lb in
      report_error (range_start, range_end) msg
  | Parser.Error ->
      let range_start = Lexing.lexeme_start_p lb in
      let range_end = Lexing.lexeme_end_p lb in
      report_error (range_start, range_end) "Syntax error"
  | Typing.Error (loc, msg) -> report_error loc msg
