(* Copyright (C) 2025 Hubert Gruniaux
 * This file is part of Iris. *)

open Tast
open LibIris
include Codegen_common
open Codegen_type
open Codegen_expr

(* C23 6.8 Statements and blocks *)
let rec codegen_stmt ctx s =
  match s.tstmt_kind with
  | Tstmt_decl d -> codegen_decl_stmt ctx d
  (* C23 6.8.2 Labeled statements *)
  | Tstmt_label (label, stmt) -> codegen_label_stmt ctx label stmt
  | Tstmt_case _ -> failwith "codegen_stmt: Case statements not implemented"
  | Tstmt_default _ ->
      failwith "codegen_stmt: Default statements not implemented"
  (* C23 6.8.3 Compound statement *)
  | Tstmt_compound stmts -> codegen_compound_stmt ctx stmts
  (* C23 6.8.4 Expression and null statements *)
  | Tstmt_null -> ()
  | Tstmt_expr e -> codegen_expression_stmt ctx e
  (* C23 6.8.5 Selection statements *)
  | Tstmt_if (cond, then_stmt, else_stmt) ->
      codegen_if_stmt ctx cond then_stmt else_stmt
  | Tstmt_switch (cond, labels, body) ->
      codegen_switch_stmt ctx cond labels body
  (* C23 6.8.6 Iteration statements *)
  | Tstmt_while (cond, body) -> codegen_while_stmt ctx cond body
  | Tstmt_do (body, cond) -> codegen_do_stmt ctx body cond
  | Tstmt_for (init_opt, cond_opt, incr_opt, body) ->
      codegen_for_stmt ctx init_opt cond_opt incr_opt body
  (* 6.8.7 Jump statements *)
  | Tstmt_goto label -> codegen_goto_stmt ctx label
  | Tstmt_continue -> codegen_continue_stmt ctx
  | Tstmt_break -> codegen_break_stmt ctx
  | Tstmt_return v -> codegen_return_stmt ctx v

and codegen_local_variable_decl ctx var_decl =
  let ir_type = codegen_type var_decl.tvar_type in

  (* Allocate space for the variable *)
  let var_ptr = Ir.Builder.emit_alloca ctx.ctx_builder ir_type in
  (match var_decl.tvar_name with
  | None -> ()
  | Some name -> Ir.Value.set_name var_ptr name.id);

  (* Codegen initializer *)
  (match var_decl.tvar_init with
  | Some init_expr ->
      let init_value = codegen_expr ctx init_expr in
      Ir.Builder.emit_store ctx.ctx_builder var_ptr init_value
  | None -> ());

  var_ptr

and codegen_decl_stmt ctx decl =
  match decl.tdecl_kind with
  | Tdecl_value value_decl ->
      let ir_val =
        match value_decl.tvalue_decl_kind with
        | Tvalue_decl_object var_decl ->
            codegen_local_variable_decl ctx var_decl
        | Tvalue_decl_function _ ->
            failwith
              "codegen_decl_stmt: Function declaration in statement not \
               implemented"
      in
      Hashtbl.add ctx.ctx_values value_decl ir_val
  | Tdecl_decls decls -> List.iter (codegen_decl_stmt ctx) decls
  | Tdecl_typedef _ -> ()

(* C23 6.8.2 Labeled statements *)
and codegen_label_stmt ctx label stmt =
  let label_bb = fresh_block ctx label.tlabel_name.id in
  Hashtbl.add ctx.ctx_labels label label_bb;
  Ir.Builder.emit_br ctx.ctx_builder label_bb;
  Ir.Builder.set_current_block ctx.ctx_builder label_bb;
  codegen_stmt ctx stmt

(* C23 6.8.3 The compound statement *)
and codegen_compound_stmt ctx stmts = List.iter (codegen_stmt ctx) stmts

(* C23 6.8.4 The expression statement *)
and codegen_expression_stmt ctx expr = ignore (codegen_expr ctx expr)

(* C23 6.8.5.2 The if statement *)
and codegen_if_stmt ctx cond then_stmt else_stmt =
  let cond_value = codegen_expr ctx cond in
  let then_bb = fresh_block ctx "if.true" in
  let else_bb = fresh_block ctx "if.false" in
  let cont_bb = fresh_block ctx "if.cont" in

  Ir.Builder.emit_br_if ctx.ctx_builder cond_value then_bb else_bb;

  Ir.Builder.set_current_block ctx.ctx_builder then_bb;
  codegen_stmt ctx then_stmt;
  Ir.Builder.emit_br ctx.ctx_builder cont_bb;

  Ir.Builder.set_current_block ctx.ctx_builder else_bb;
  Option.iter (codegen_stmt ctx) else_stmt;
  Ir.Builder.emit_br ctx.ctx_builder cont_bb;

  Ir.Builder.set_current_block ctx.ctx_builder cont_bb

(* C23 6.8.5.3 The switch statement *)
and codegen_switch_stmt ctx cond labels body =
  ignore ctx;
  ignore cond;
  ignore labels;
  ignore body;
  failwith "codegen_switch_stmt: Not implemented"

(* C23 6.8.6.2 The while statement *)
and codegen_while_stmt ctx cond body =
  let header_bb = fresh_block ctx "while.header" in
  let body_bb = fresh_block ctx "while.body" in
  let cont_bb = fresh_block ctx "while.cont" in

  Ir.Builder.emit_br ctx.ctx_builder header_bb;
  Ir.Builder.set_current_block ctx.ctx_builder header_bb;
  let cond_value = codegen_expr ctx cond in
  Ir.Builder.emit_br_if ctx.ctx_builder cond_value body_bb cont_bb;
  Ir.Builder.set_current_block ctx.ctx_builder body_bb;
  push_loop_labels ctx cont_bb header_bb;
  codegen_stmt ctx body;
  pop_loop_labels ctx;
  Ir.Builder.emit_br ctx.ctx_builder header_bb;
  Ir.Builder.set_current_block ctx.ctx_builder cont_bb

(* C23 6.8.6.3 The do statement *)
and codegen_do_stmt ctx body cond =
  let body_bb = fresh_block ctx "do.body" in
  let header_bb = fresh_block ctx "do.header" in
  let cont_bb = fresh_block ctx "do.cont" in

  Ir.Builder.emit_br ctx.ctx_builder body_bb;
  Ir.Builder.set_current_block ctx.ctx_builder body_bb;
  push_loop_labels ctx cont_bb header_bb;
  codegen_stmt ctx body;
  pop_loop_labels ctx;
  Ir.Builder.emit_br ctx.ctx_builder header_bb;
  Ir.Builder.set_current_block ctx.ctx_builder header_bb;
  let cond_value = codegen_expr ctx cond in
  Ir.Builder.emit_br_if ctx.ctx_builder cond_value body_bb cont_bb;
  Ir.Builder.set_current_block ctx.ctx_builder cont_bb

(* C23 6.8.6.4 The for statement *)
and codegen_for_stmt ctx init_opt cond_opt incr_opt body =
  let header_bb = fresh_block ctx "for.header" in
  let body_bb = fresh_block ctx "for.body" in
  let incr_bb = fresh_block ctx "for.incr" in
  let cont_bb = fresh_block ctx "for.cont" in

  Option.iter (codegen_stmt ctx) init_opt;
  Ir.Builder.emit_br ctx.ctx_builder header_bb;

  Ir.Builder.set_current_block ctx.ctx_builder header_bb;
  (match cond_opt with
  | Some cond ->
      let cond_value = codegen_expr ctx cond in
      Ir.Builder.emit_br_if ctx.ctx_builder cond_value body_bb cont_bb
  | None -> Ir.Builder.emit_br ctx.ctx_builder body_bb);

  Ir.Builder.set_current_block ctx.ctx_builder body_bb;
  push_loop_labels ctx cont_bb incr_bb;
  codegen_stmt ctx body;
  pop_loop_labels ctx;
  Ir.Builder.emit_br ctx.ctx_builder incr_bb;

  Ir.Builder.set_current_block ctx.ctx_builder incr_bb;
  Option.iter (codegen_stmt ctx) incr_opt;
  Ir.Builder.emit_br ctx.ctx_builder header_bb;

  Ir.Builder.set_current_block ctx.ctx_builder cont_bb

(* C23 6.8.7.2 The goto statement *)
and codegen_goto_stmt ctx label =
  let target_bb = Hashtbl.find ctx.ctx_labels label in
  Ir.Builder.emit_br ctx.ctx_builder target_bb

(* C23 6.8.7.3 The continue statement *)
and codegen_continue_stmt ctx =
  let loop_labels = List.hd ctx.ctx_loop_labels in
  Ir.Builder.emit_br ctx.ctx_builder loop_labels.continue_bb

(* C23 6.8.7.4 The break statement *)
and codegen_break_stmt ctx =
  (* FIXME: Break statement in within statement not handled. *)
  let loop_labels = List.hd ctx.ctx_loop_labels in
  Ir.Builder.emit_br ctx.ctx_builder loop_labels.break_bb

(* C23 6.8.7.5 The return statement *)
and codegen_return_stmt ctx v =
  let tv = Option.map (codegen_expr ctx) v in
  Ir.Builder.emit_ret ctx.ctx_builder tv

let codegen_function_declaration ctx value_decl func_decl =
  let name = func_decl.tfun_name.id in
  let ir_function_type = codegen_type func_decl.tfun_type in
  let is_external = Option.is_none func_decl.tfun_body in

  (if is_external then
     let global =
       Ir.Builder.emit_extern_function ctx.ctx_builder name ir_function_type
     in
     Hashtbl.add ctx.ctx_values value_decl (Ir.Ival_global global)
   else
     let global, ir_params =
       Ir.Builder.begin_function ctx.ctx_builder name ir_function_type
     in

     List.iter2
       (fun param_decl ir_param ->
         match param_decl.tvalue_decl_name with
         | Some id ->
             let ir_param_addr =
               Ir.Builder.emit_alloca ctx.ctx_builder (Ir.Reg.type_of ir_param)
             in
             Ir.Builder.emit_store ctx.ctx_builder ir_param_addr
               (Ir.Ival_reg ir_param);
             Ir.Reg.set_name ir_param id.id;
             Ir.Value.set_name ir_param_addr (id.id ^ ".addr");
             Hashtbl.add ctx.ctx_values param_decl ir_param_addr
         | None -> ())
       func_decl.tfun_params ir_params;

     Hashtbl.add ctx.ctx_values value_decl (Ir.Ival_global global);
     Option.iter (codegen_stmt ctx) func_decl.tfun_body;
     Ir.Builder.end_function ctx.ctx_builder);
  ()

let codegen_value_declaration ctx value_decl =
  match value_decl.tvalue_decl_kind with
  | Tvalue_decl_function func_decl ->
      codegen_function_declaration ctx value_decl func_decl
  | Tvalue_decl_object _ ->
      failwith
        "codegen_value_declaration: Tvalue_decl_object: Not implemented \
         (global variables)"

let rec codegen_declaration ctx decl =
  match decl.tdecl_kind with
  | Tdecl_value value_decl -> codegen_value_declaration ctx value_decl
  | Tdecl_typedef _ -> ()
  | Tdecl_decls decls -> List.iter (codegen_declaration ctx) decls

let codegen_translation_unit tu =
  let ir_module = Ir.Module.create () in
  let ctx =
    {
      ctx_module = ir_module;
      ctx_builder = Ir.Builder.create ir_module;
      ctx_values = Hashtbl.create 7;
      ctx_labels = Hashtbl.create 7;
      ctx_loop_labels = [];
      ctx_type_cache = Hashtbl.create 7;
    }
  in

  List.iter (codegen_declaration ctx) tu;

  ctx.ctx_module
