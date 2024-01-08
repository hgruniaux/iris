open LibIris
open Ir

let _ = 5 :: 4 :: []

let () =
  let ib = IrBuilder.create () in
  let main = IrBuilder.mk_func ib "main" 0 in
  let a = IrBuilder.mk_const ib 0l in
  let b = IrBuilder.mk_const ib 1l in
  let r2 = IrBuilder.mk_load ib a in
  let _ = IrBuilder.mk_add ib b r2 in
  dump_dot main
