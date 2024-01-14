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
  | Eint i -> IrBuilder.mk_int ib i
  | Estring s -> IrBuilder.mk_zstring ib s
  | Evar name -> (
      match Hashtbl.find_opt env.vars name with
      | Some reg -> reg
      | None -> assert false)
  | Enew s -> IrBuilder.mk_malloc ib s
  | Ebinop (Bland, lhs, rhs) ->
      let l = compile_expr env ib lhs in
      IrBuilder.mk_logical_and ib l (fun ib -> compile_expr env ib rhs)
  | Ebinop (Blor, lhs, rhs) ->
      let l = compile_expr env ib lhs in
      IrBuilder.mk_logical_or ib l (fun ib -> compile_expr env ib rhs)
  | Ebinop (op, lhs, rhs) -> (
      let l = compile_expr env ib lhs in
      let r = compile_expr env ib rhs in
      match op with
      | Badd -> IrBuilder.mk_add ib l r
      | Bsub -> IrBuilder.mk_sub ib l r
      | Bmul -> IrBuilder.mk_mul ib l r
      | Bdiv -> IrBuilder.mk_sdiv ib l r
      | Brem -> IrBuilder.mk_srem ib l r
      | Band -> IrBuilder.mk_and ib l r
      | Bor -> IrBuilder.mk_or ib l r
      | Bxor -> IrBuilder.mk_xor ib l r
      | Bland | Blor -> assert false (* already handled *)
      | Bshift_left -> IrBuilder.mk_lsl ib l r
      | Bshift_right -> IrBuilder.mk_asr ib l r
      | Beq -> IrBuilder.mk_eq ib l r
      | Bne -> IrBuilder.mk_ne ib l r
      | Blt -> IrBuilder.mk_slt ib l r
      | Ble -> IrBuilder.mk_sle ib l r
      | Bgt -> IrBuilder.mk_sgt ib l r
      | Bge -> IrBuilder.mk_sge ib l r)
  | Eunop (op, sub) -> (
      let s = compile_expr env ib sub in
      match op with
      | Uneg -> IrBuilder.mk_neg ib s
      | Unot -> IrBuilder.mk_not ib s)
  | Ecall (name, args) ->
      let cargs =
        List.map (fun arg -> Ir.Iop_reg (compile_expr env ib arg)) args
      in
      let fn = Hashtbl.find ib.ctx.ctx_symbol_table name in
      IrBuilder.mk_call ib fn cargs

and compile_stmt env ib s =
  match s with
  | Sempty -> ()
  | Sexpr e -> ignore (compile_expr env ib e)
  | Svardecl (n, e) ->
      let value = compile_expr env ib e in
      Hashtbl.add env.vars n value
  | Sprint e ->
      let value = compile_expr env ib e in
      let typ = Ir.type_of_reg (Option.get ib.cur_func_decl) value in
      if typ = Ir.Ityp_ptr then
        let puts_fn =
          Ir.get_or_insert_fn ib.ctx "puts" ~is_external:true Ir.Ityp_void
            [ Ir.Ityp_ptr ]
        in
        ignore (IrBuilder.mk_call ib puts_fn [ Ir.Iop_reg value ])
      else
        let printf_fn =
          Ir.get_or_insert_fn ib.ctx "printf" ~is_external:true Ir.Ityp_void
            [ Ir.Ityp_ptr; Ir.Ityp_int ]
        in
        let format = IrBuilder.mk_zstring ib "%d\n" in
        ignore (IrBuilder.mk_call ib printf_fn [ Ir.Iop_reg format; Ir.Iop_reg value ])
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
  | Sdelete e ->
      let ptr = compile_expr env ib e in
      ignore (IrBuilder.mk_free ib ptr)
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

and compile_func ib (name, params, body) =
  let func_decl = IrBuilder.mk_fn ib name (List.length params) in
  let env = { vars = Hashtbl.create 17; labels = Hashtbl.create 17 } in
  List.iter2
    (fun name reg -> Hashtbl.add env.vars name reg)
    params func_decl.fn_params;
  ignore (compile_stmt env ib body);
  IrBuilder.finish ib;

  func_decl
