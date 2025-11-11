include Ir_base
module Value = Ir_value
module ValueSet = Set.Make (Value)
module ValueMap = Map.Make (Value)
module Instruction = Ir_instruction
module Terminator = Ir_terminator
module Block = Ir_block
module Function = Ir_function
module Module = Ir_module
module Builder = Ir_builder
module Printer = Ir_printer
module Cfg = Ir_cfg
module CallGraph = Ir_callgraph
