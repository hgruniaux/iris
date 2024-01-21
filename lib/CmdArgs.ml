let spec =
  [
    ("--opt", Arg.Set PassManager.optimize, "  enable optimizations IR");
    ("--dump-ir", Arg.Set PassManager.dump_ir, "  dumps generated IR");
    ( "--dump-ir-dot",
      Arg.Set PassManager.dump_ir_dot,
      "  dumps generated IR in Graphviz DOT format" );
    ("--dump-mir", Arg.Set PassManager.dump_mir, "  dumps generated machine MIR");
    ( "--dump-liveinfo",
      Arg.Set PassManager.dump_liveinfo,
      "  dumps the liveness analysis result" );
    ( "--dump-interf",
      Arg.Set PassManager.dump_interf,
      "  dumps the computed interference graph in Graphviz DOT format" );
    ( "--dump-reg-alloc",
      Arg.Set PassManager.dump_reg_alloc,
      "  dumps the register allocation result" );
      ( "--dump-callgraph",
        Arg.Set PassManager.dump_callgraph,
        "  dumps the computed callgraph" );
  ]
