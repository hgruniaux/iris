open Ast
open LibIris
module SMap = Map.Make (String)

exception Error of string

type env = {
  vars : (string, Ir.reg) Hashtbl.t;
  labels : (string, Ir.label) Hashtbl.t;
}

let rec compile_expr env ib e =
  match e with
  | Econst i -> IrBuilder.mk_const ib i
  | Evar name -> (
      match Hashtbl.find_opt env.vars name with
      | Some reg -> reg
      | None -> assert false)
  | Ebinop (op, lhs, rhs) -> (
      let l = compile_expr env ib lhs in
      let r = compile_expr env ib rhs in
      match op with
      | Badd -> IrBuilder.mk_add ib l r
      | Bsub -> IrBuilder.mk_sub ib l r
      | Bmul -> IrBuilder.mk_mul ib l r
      | Bdiv -> IrBuilder.mk_sdiv ib l r
      | Brem -> IrBuilder.mk_srem ib l r)
  | Eunop (op, sub) -> (
      let s = compile_expr env ib sub in
      match op with Iunop_neg -> IrBuilder.mk_neg ib s)
  | Ecall (name, args) ->
      let cargs = List.map (fun arg -> Ir.Iop_reg (compile_expr env ib arg)) args in
      let fn = Hashtbl.find ib.ctx.ctx_symbol_table name in
      IrBuilder.mk_call ib fn cargs

and compile_stmt env ib s =
  match s with
  | Sempty -> ()
  | Sexpr e -> ignore (compile_expr env ib e)
  | Slabel (name, stmt) ->
    let bb = IrBuilder.mk_bb ib in
    Hashtbl.add env.labels name bb.b_label;
    IrBuilder.set_terminator ib (Ir.Iinst_jmp bb.b_label);
    IrBuilder.set_bb ib bb;
    compile_stmt env ib stmt
  | Sreturn e ->
      (match e with
      | None -> IrBuilder.set_terminator ib Ir.Iinst_ret
      | Some e ->
          let ret_val = compile_expr env ib e in
          IrBuilder.set_terminator ib (Ir.Iinst_retv (Iop_reg ret_val)));
      let bb = IrBuilder.mk_bb ib in
      IrBuilder.set_bb ib bb
  | Sbreak -> IrBuilder.mk_break ib
  | Scontinue -> IrBuilder.mk_continue ib
  | Sgoto i -> (
    match Hashtbl.find_opt env.labels i with
    | Some l ->
      IrBuilder.set_terminator ib (Iinst_jmp l);
      let bb = IrBuilder.mk_bb ib in
      IrBuilder.set_bb ib bb
    | None -> raise (Error ("Unknown label " ^ i)))
  | Sblock s -> List.iter (compile_stmt env ib) s
  | Sif (cond, then_stmt, else_stmt) ->
      let then_bb = IrBuilder.mk_bb ib in
      let else_bb = IrBuilder.mk_bb ib in
      let end_bb = IrBuilder.mk_bb ib in
      let cond_val = compile_expr env ib cond in
      IrBuilder.set_terminator ib
        (Iinst_jmpc (Iop_reg cond_val, then_bb.b_label, else_bb.b_label));
      IrBuilder.set_bb ib then_bb;
      compile_stmt env ib then_stmt;
      IrBuilder.set_terminator ib (Iinst_jmp end_bb.b_label);
      IrBuilder.set_bb ib else_bb;
      compile_stmt env ib else_stmt;
      IrBuilder.set_terminator ib (Iinst_jmp end_bb.b_label);
      IrBuilder.set_bb ib end_bb
  | Swhile (cond, body) ->
      let cond_bb = IrBuilder.mk_bb ib in
      let loop_bb = IrBuilder.mk_bb ib in
      let end_bb = IrBuilder.mk_bb ib in
      IrBuilder.set_terminator ib (Iinst_jmp cond_bb.b_label);
      IrBuilder.set_bb ib cond_bb;
      let cond_val = compile_expr env ib cond in
      IrBuilder.set_terminator ib
        (Iinst_jmpc (Iop_reg cond_val, loop_bb.b_label, end_bb.b_label));
      IrBuilder.set_bb ib loop_bb;
      IrBuilder.enter_loop ib cond_bb.b_label end_bb.b_label;
      compile_stmt env ib body;
      IrBuilder.exit_loop ib;
      IrBuilder.set_terminator ib (Iinst_jmp cond_bb.b_label);
      IrBuilder.set_bb ib end_bb
  | Sloop body -> IrBuilder.mk_loop ib (fun ib -> compile_stmt env ib body)

and compile_func (name, params, body) =
  let ctx = Ir.mk_ctx () in
  let ib = IrBuilder.create ctx in
  let func_decl = IrBuilder.mk_fn ib name (List.length params) in
  let env =
    {
      vars = Hashtbl.create 17;
      labels = Hashtbl.create 17;
    }
  in
  List.iter2 (fun name reg -> Hashtbl.add env.vars name reg) params func_decl.fn_params;
  ignore (compile_stmt env ib body);

  func_decl
