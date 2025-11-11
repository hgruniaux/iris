(*
 * Copyright (c) 2025 Hubert Gruniaux
 * This file is part of the Iris project.
 *)

open Ir

type context = {
  ctx_smt_ctx : Z3.context;
  ctx_smt_solver : Z3.Solver.solver;
  ctx_unit_sort : Z3.Sort.sort;
  ctx_unit_const : Z3.Expr.expr;
  ctx_mem_sort : Z3.Sort.sort;
  ctx_globals_map : (global, Z3.Expr.expr) Hashtbl.t;
}

type encoded_function = {
  constraints_fn : Z3.FuncDecl.func_decl;
      (** [constraints_fn] is a Z3 recursive function definition taking a memory
          state as a first argument and then the modeled function's arguments,
          returning a boolean indicating whether the constraints hold. *)
  ubs_fn : Z3.FuncDecl.func_decl;
      (** [ubs_fn] is a Z3 recursive function definition taking a memory state
          as a first argument and then the modeled function's arguments,
          returning a boolean indicating whether any undefined behavior occurs.
      *)
  return_fn : Z3.FuncDecl.func_decl;
      (** [return_fn] is a Z3 recursive function definition taking a memory
          state as a first argument and then the modeled function's arguments,
          returning the function's return value. *)
}

type equivalence_fail_info = {
  memory : Z3.Expr.expr;
      (** The initial memory state for which the two functions differ. *)
  args : Z3.Expr.expr list;
      (** The arguments for which the two functions differ, in the original
          order of the IR function. *)
  src_return : Z3.Expr.expr;
      (** The return value of the source function for the given [args]. *)
  tgt_return : Z3.Expr.expr;
      (** The return value of the target function for the given [args]. *)
  src_has_ub : bool;
      (** Whether the source function exhibits undefined behavior for the given
          [args]. *)
  tgt_has_ub : bool;
      (** Whether the target function exhibits undefined behavior for the given
          [args]. *)
}

type equivalence_result =
  | Equivalent
  | Unknown of string
  | Not_equivalent of equivalence_fail_info

type function_context = {
  fctx_ctx : context;
  fctx_regs_map : (Reg.t, Z3.Expr.expr) Hashtbl.t;
      (** Map from register identifiers to their SMT expressions. *)
  fctx_function : fn;  (** The function being analyzed. *)
  fctx_reachable_blocks : (Label.t, Z3.Expr.expr) Hashtbl.t;
      (** Map from basic block labels to boolean expressions indicating whether
          the block is reachable. *)
  fctx_preds_conditions : (Label.t, Z3.Expr.expr list) Hashtbl.t;
      (** Map from basic block labels to the list of conditions under which the
          block is reachable from its predecessors. *)
  mutable fctx_allocas : (Z3.Expr.expr * int) list;
      (** List of alloca addresses allocated in the function. *)
  mutable fctx_constraints : Z3.Expr.expr list;
      (** List of constraints that must hold for the function. *)
  mutable fctx_ubs : Z3.Expr.expr list;
      (** List of undefined behavior conditions for the function. *)
  mutable fctx_return_value : Z3.Expr.expr option;
      (** SMT expression representing the function's return value. *)
}

(** Adds the SMT expression [condition] as a constraint that must hold for
    [func_ctx]. *)
let add_constraint func_ctx condition =
  func_ctx.fctx_constraints <- condition :: func_ctx.fctx_constraints

(** Adds the SMT expression [condition] as an undefined behavior condition for
    [func_ctx]. *)
let add_undefined_behavior func_ctx current_block condition =
  let block_reachable =
    Hashtbl.find func_ctx.fctx_reachable_blocks current_block.block_label
  in
  let ub_condition =
    Z3.Boolean.mk_and func_ctx.fctx_ctx.ctx_smt_ctx
      [ block_reachable; condition ]
  in
  func_ctx.fctx_ubs <- ub_condition :: func_ctx.fctx_ubs

(** Adds an alloca address and its block size (in bytes) to [func_ctx]. *)
let add_alloca func_ctx addr size =
  func_ctx.fctx_allocas <- (addr, size) :: func_ctx.fctx_allocas

let add_unreachable _func_ctx _condition = ()

let add_return func_ctx condition value =
  match func_ctx.fctx_return_value with
  | Some previous_return_value ->
      let smt_ctx = func_ctx.fctx_ctx.ctx_smt_ctx in
      let new_return_value =
        Z3.Boolean.mk_ite smt_ctx condition value previous_return_value
      in
      func_ctx.fctx_return_value <- Some new_return_value
  | None -> func_ctx.fctx_return_value <- Some value

(** Encode an IR type into an Z3 sort. *)
let rec encode_type ctx ty =
  match ty with
  | Ityp_unit -> ctx.ctx_unit_sort
  | Ityp_i1 -> Z3.BitVector.mk_sort ctx.ctx_smt_ctx 1
  | Ityp_i8 -> Z3.BitVector.mk_sort ctx.ctx_smt_ctx 8
  | Ityp_i16 -> Z3.BitVector.mk_sort ctx.ctx_smt_ctx 16
  | Ityp_i32 -> Z3.BitVector.mk_sort ctx.ctx_smt_ctx 32
  | Ityp_i64 -> Z3.BitVector.mk_sort ctx.ctx_smt_ctx 64
  | Ityp_f32 -> Z3.FloatingPoint.mk_sort_32 ctx.ctx_smt_ctx
  | Ityp_f64 -> Z3.FloatingPoint.mk_sort_64 ctx.ctx_smt_ctx
  | Ityp_ptr -> encode_type ctx Machine_info.pointer_integer_type
  | _ -> failwith "Unsupported type for SMT encoding"

let create_context smt_ctx =
  let unit_sort = Z3.Boolean.mk_sort smt_ctx in
  let unit_const = Z3.Boolean.mk_true smt_ctx in
  let mem_sort =
    Z3.Z3Array.mk_sort smt_ctx
      (Z3.BitVector.mk_sort smt_ctx 64) (* FIXME *)
      (Z3.BitVector.mk_sort smt_ctx 8)
  in
  {
    ctx_smt_ctx = smt_ctx;
    ctx_smt_solver = Z3.Solver.mk_simple_solver smt_ctx;
    ctx_unit_sort = unit_sort;
    ctx_unit_const = unit_const;
    ctx_mem_sort = mem_sort;
    ctx_globals_map = Hashtbl.create 17;
  }

let create_function_context ctx fn =
  let function_ctx =
    {
      fctx_ctx = ctx;
      fctx_regs_map = Hashtbl.create 17;
      fctx_function = fn;
      fctx_reachable_blocks = Hashtbl.create 17;
      fctx_preds_conditions = Hashtbl.create 17;
      fctx_allocas = [];
      fctx_constraints = [];
      fctx_ubs = [];
      fctx_return_value = None;
    }
  in

  LabelMap.iter
    (fun label _bb ->
      let reachable_var =
        Z3.Expr.mk_fresh_const function_ctx.fctx_ctx.ctx_smt_ctx
          (Format.asprintf "block_reachable_%a" Label.pp_print label)
          (Z3.Boolean.mk_sort function_ctx.fctx_ctx.ctx_smt_ctx)
      in
      Hashtbl.add function_ctx.fctx_reachable_blocks label reachable_var)
    fn.fn_blocks;

  let entry_label = Option.get fn.fn_entry in
  add_constraint function_ctx
    (Z3.Boolean.mk_eq ctx.ctx_smt_ctx
       (Hashtbl.find function_ctx.fctx_reachable_blocks entry_label)
       (Z3.Boolean.mk_true ctx.ctx_smt_ctx));

  function_ctx

(** Encode an IR value into an SMT expression. *)
let encode_value ctx v =
  match v with
  | Ival_int (ty, c) ->
      let sort = encode_type ctx.fctx_ctx ty in
      let bitwidth = Z3.BitVector.get_size sort in
      Z3.BitVector.mk_numeral ctx.fctx_ctx.ctx_smt_ctx (Z.to_string c) bitwidth
  | Ival_float (ty, f) ->
      let sort = encode_type ctx.fctx_ctx ty in
      Z3.FloatingPoint.mk_numeral_f ctx.fctx_ctx.ctx_smt_ctx f sort
  | Ival_reg reg -> Hashtbl.find ctx.fctx_regs_map reg
  | Ival_global g -> Hashtbl.find ctx.fctx_ctx.ctx_globals_map g

(** Convert a list of byte expressions into a value of the given type,
    respecting target system endianness. *)
let value_from_bytes ctx ty bytes_exprs =
  let smt_ctx = ctx.fctx_ctx.ctx_smt_ctx in

  (* Concatenate the byte expressions into a single bitvector. *)
  let concat_bytes bytes =
    match bytes with
    | [] -> failwith "load_value_from_bytes: empty byte list"
    | b0 :: bs ->
        List.fold_left (fun acc b -> Z3.BitVector.mk_concat smt_ctx acc b) b0 bs
  in

  (* Adjust byte order according to system endianness *)
  let ordered_bytes =
    match Machine_info.system_endianess with
    | Little_endian -> List.rev bytes_exprs
    | Big_endian -> bytes_exprs
  in

  match ty with
  | Ityp_i1 ->
      let byte_expr = List.hd bytes_exprs in
      Z3.BitVector.mk_extract smt_ctx 0 0 byte_expr
  | Ityp_i8 -> List.hd bytes_exprs
  | Ityp_i16 -> concat_bytes (List.take 2 ordered_bytes)
  | Ityp_i32 -> concat_bytes (List.take 4 ordered_bytes)
  | Ityp_i64 -> concat_bytes (List.take 8 ordered_bytes)
  | Ityp_f32 ->
      let bv = concat_bytes (List.take 4 ordered_bytes) in
      Z3.FloatingPoint.mk_to_fp_bv smt_ctx bv
        (Z3.FloatingPoint.mk_sort_32 smt_ctx)
  | Ityp_f64 ->
      let bv = concat_bytes (List.take 8 ordered_bytes) in
      Z3.FloatingPoint.mk_to_fp_bv smt_ctx bv
        (Z3.FloatingPoint.mk_sort_64 smt_ctx)
  | _ -> failwith "Unsupported type for load_value_from_bytes"

(** Convert a value of the given type into a list of byte expressions,
    respecting target system endianness. *)
let value_to_bytes ctx ty value_expr =
  let smt_ctx = ctx.fctx_ctx.ctx_smt_ctx in
  let byte_size = Machine_info.size_of ty in

  let rec extract_bytes acc byte_index =
    if byte_index >= byte_size then List.rev acc
    else
      let byte_expr =
        Z3.BitVector.mk_extract smt_ctx
          ((byte_index * 8) + 7)
          (byte_index * 8) value_expr
      in
      extract_bytes (byte_expr :: acc) (byte_index + 1)
  in

  let bytes = extract_bytes [] 0 in

  (* Adjust byte order according to system endianness *)
  match Machine_info.system_endianess with
  | Little_endian -> bytes
  | Big_endian -> List.rev bytes

let encode_alignment smt_ctx addr align =
  let align_int = Z3.Arithmetic.Integer.mk_numeral_i smt_ctx align in
  let zero = Z3.Arithmetic.Integer.mk_numeral_i smt_ctx 0 in
  Z3.Boolean.mk_eq smt_ctx
    (Z3.Arithmetic.Integer.mk_mod smt_ctx addr align_int)
    zero

(** Encodes [expr] into an SMT expression under [fctx] and [mem]. It returns the
    SMT expression and the updated memory. *)
let rec encode_expr fctx current_block mem expr =
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  match expr with
  | Iexpr_value v -> (encode_value fctx v, mem)
  | Iexpr_ibinop (op, lhs, rhs) ->
      (encode_ibinary fctx current_block op lhs rhs, mem)
  | Iexpr_iunop (op, operand) -> (encode_iunary fctx op operand, mem)
  | Iexpr_icmp (op, lhs, rhs) -> (encode_icmp fctx op lhs rhs, mem)
  | Iexpr_load (ty, addr) ->
      let smt_addr = encode_value fctx addr in
      let byte_size = Machine_info.size_of ty in

      let addr_bw = Z3.BitVector.get_size (Z3.Expr.get_sort smt_addr) in
      let loaded_bytes =
        List.init byte_size (fun i ->
            let offset =
              Z3.BitVector.mk_numeral smt_ctx (string_of_int i) addr_bw
            in
            let smt_byte_addr = Z3.BitVector.mk_add smt_ctx smt_addr offset in
            Z3.Z3Array.mk_select smt_ctx mem smt_byte_addr)
      in

      let loaded_value = value_from_bytes fctx ty loaded_bytes in

      (loaded_value, mem)
  | Iexpr_alloca (ty, align) ->
      let ptr_bw = Z3.BitVector.get_size (encode_type fctx.fctx_ctx Ityp_ptr) in
      let int_sort = Z3.Arithmetic.Integer.mk_sort smt_ctx in
      let addr = Z3.Expr.mk_fresh_const smt_ctx "alloca_addr" int_sort in
      add_constraint fctx (encode_alignment smt_ctx addr align);
      add_alloca fctx addr (Machine_info.size_of ty);
      (Z3.Arithmetic.Integer.mk_int2bv smt_ctx ptr_bw addr, mem)
  | _ -> failwith "SMT encoding for this expression not yet implemented"

and add_div_ub fctx current_block lhs rhs =
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  let bw = Z3.BitVector.get_size (Z3.Expr.get_sort lhs) in
  add_undefined_behavior fctx current_block
    (Z3.Boolean.mk_eq smt_ctx rhs (Z3.BitVector.mk_numeral smt_ctx "0" bw))

and add_shift_ub fctx current_block lhs rhs =
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  let bw = Z3.BitVector.get_size (Z3.Expr.get_sort lhs) in
  add_undefined_behavior fctx current_block
    (Z3.BitVector.mk_uge smt_ctx rhs
       (Z3.BitVector.mk_numeral smt_ctx (string_of_int bw) bw))

and encode_ibinary fctx current_block op lhs rhs =
  let lhs = encode_value fctx lhs in
  let rhs = encode_value fctx rhs in
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  match op with
  | Ibinop_add -> Z3.BitVector.mk_add smt_ctx lhs rhs
  | Ibinop_sub -> Z3.BitVector.mk_sub smt_ctx lhs rhs
  | Ibinop_mul -> Z3.BitVector.mk_mul smt_ctx lhs rhs
  | Ibinop_div_u ->
      add_div_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_udiv smt_ctx lhs rhs
  | Ibinop_div_s ->
      add_div_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_sdiv smt_ctx lhs rhs
  | Ibinop_rem_u ->
      add_div_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_urem smt_ctx lhs rhs
  | Ibinop_rem_s ->
      add_div_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_srem smt_ctx lhs rhs
  | Ibinop_and -> Z3.BitVector.mk_and smt_ctx lhs rhs
  | Ibinop_or -> Z3.BitVector.mk_or smt_ctx lhs rhs
  | Ibinop_xor -> Z3.BitVector.mk_xor smt_ctx lhs rhs
  | Ibinop_lsl ->
      add_shift_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_shl smt_ctx lhs rhs
  | Ibinop_lsr ->
      add_shift_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_lshr smt_ctx lhs rhs
  | Ibinop_asr ->
      add_shift_ub fctx current_block lhs rhs;
      Z3.BitVector.mk_ashr smt_ctx lhs rhs

and encode_iunary fctx op operand =
  let operand = encode_value fctx operand in
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  match op with
  | Iunop_neg -> Z3.BitVector.mk_neg smt_ctx operand
  | Iunop_not -> Z3.BitVector.mk_not smt_ctx operand

and encode_icmp fctx op lhs rhs =
  let lhs = encode_value fctx lhs in
  let rhs = encode_value fctx rhs in
  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
  let to_bv v =
    let true_bv = Z3.BitVector.mk_numeral smt_ctx "1" 1 in
    let false_bv = Z3.BitVector.mk_numeral smt_ctx "0" 1 in
    Z3.Boolean.mk_ite smt_ctx v true_bv false_bv
  in

  match op with
  | Icmp_eq -> to_bv (Z3.Boolean.mk_eq smt_ctx lhs rhs)
  | Icmp_ne ->
      to_bv (Z3.Boolean.mk_not smt_ctx (Z3.Boolean.mk_eq smt_ctx lhs rhs))
  | Icmp_lt_u -> to_bv (Z3.BitVector.mk_ult smt_ctx lhs rhs)
  | Icmp_le_u -> to_bv (Z3.BitVector.mk_ule smt_ctx lhs rhs)
  | Icmp_gt_u -> to_bv (Z3.BitVector.mk_ugt smt_ctx lhs rhs)
  | Icmp_ge_u -> to_bv (Z3.BitVector.mk_uge smt_ctx lhs rhs)
  | Icmp_lt_s -> to_bv (Z3.BitVector.mk_slt smt_ctx lhs rhs)
  | Icmp_le_s -> to_bv (Z3.BitVector.mk_sle smt_ctx lhs rhs)
  | Icmp_gt_s -> to_bv (Z3.BitVector.mk_sgt smt_ctx lhs rhs)
  | Icmp_ge_s -> to_bv (Z3.BitVector.mk_sge smt_ctx lhs rhs)

let encode_instruction fctx current_block mem instruction =
  match instruction with
  | Iinst_def (name, expr) ->
      let smt_expr, new_mem = encode_expr fctx current_block mem expr in
      Hashtbl.add fctx.fctx_regs_map name smt_expr;
      new_mem
  | Iinst_store (addr, value) ->
      let smt_addr = encode_value fctx addr in
      let smt_value = encode_value fctx value in
      let bytes = value_to_bytes fctx (Value.type_of value) smt_value in
      fst
        (List.fold_left
           (fun (mem, i) byte_expr ->
             let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in
             let addr_bw = Z3.BitVector.get_size (Z3.Expr.get_sort smt_addr) in
             let offset =
               Z3.BitVector.mk_numeral smt_ctx (string_of_int i) addr_bw
             in
             let smt_byte_addr = Z3.BitVector.mk_add smt_ctx smt_addr offset in
             (Z3.Z3Array.mk_store smt_ctx mem smt_byte_addr byte_expr, i + 1))
           (mem, 0) bytes)

let encode_terminator fctx current_block term =
  let current_block_reachable =
    Hashtbl.find fctx.fctx_reachable_blocks current_block.block_label
  in

  let smt_ctx = fctx.fctx_ctx.ctx_smt_ctx in

  let encode_branch cond target_label args =
    let target_block = Function.find_block fctx.fctx_function target_label in
    let args_encoded =
      List.map2
        (fun param arg ->
          let param_encoded = Hashtbl.find fctx.fctx_regs_map param in
          let arg_encoded = encode_value fctx arg in
          Z3.Boolean.mk_eq smt_ctx param_encoded arg_encoded)
        (Block.params target_block)
        args
    in
    let target_reachable =
      Hashtbl.find fctx.fctx_reachable_blocks target_label
    in
    Z3.Boolean.mk_implies smt_ctx cond
      (Z3.Boolean.mk_and smt_ctx (target_reachable :: args_encoded))
  in

  let add_branch cond_opt target_label args =
    let cond =
      match cond_opt with
      | Some c -> Z3.Boolean.mk_and smt_ctx [ current_block_reachable; c ]
      | None -> current_block_reachable
    in
    let branch_constraint = encode_branch cond target_label args in

    match Hashtbl.find_opt fctx.fctx_preds_conditions target_label with
    | Some conds ->
        Hashtbl.replace fctx.fctx_preds_conditions target_label (cond :: conds)
    | None ->
        Hashtbl.add fctx.fctx_preds_conditions target_label [ cond ];

        add_constraint fctx branch_constraint
  in

  match term with
  | Iterm_br (target_label, args) -> add_branch None target_label args
  | Iterm_br_if (cond, true_label, true_args, false_label, false_args) ->
      let cond =
        Z3.Boolean.mk_eq smt_ctx (encode_value fctx cond)
          (Z3.BitVector.mk_numeral smt_ctx "1" 1)
      in
      (* True branch *)
      add_branch (Some cond) true_label true_args;
      (* False branch *)
      add_branch (Some (Z3.Boolean.mk_not smt_ctx cond)) false_label false_args
  | Iterm_br_table (_index, _default_label, _default_args, _cases) ->
      failwith "SMT encoding for br_table not yet implemented"
  | Iterm_ret ret_val_opt ->
      let value =
        match ret_val_opt with
        | Some v -> encode_value fctx v
        | None -> fctx.fctx_ctx.ctx_unit_const
      in
      add_return fctx current_block_reachable value
  | Iterm_unreachable -> add_unreachable fctx current_block_reachable

let encode_block fctx mem block =
  let mem =
    List.fold_left
      (fun old_mem inst -> encode_instruction fctx block old_mem inst)
      mem block.block_insts
  in
  encode_terminator fctx block block.block_term;
  mem

let encode_function ctx fn =
  let function_context = create_function_context ctx fn in

  (* We add an additional parameter for the memory state. *)
  let params_types =
    ctx.ctx_mem_sort :: List.map (encode_type ctx) (Function.params_types_of fn)
  in

  let initial_mem =
    Z3.Expr.mk_fresh_const ctx.ctx_smt_ctx "memory" ctx.ctx_mem_sort
  in
  let params_encoded =
    initial_mem
    :: List.mapi
         (fun i param ->
           let smt_param =
             Z3.Expr.mk_fresh_const ctx.ctx_smt_ctx
               (Format.asprintf "arg_%d" i)
               (encode_type ctx (Reg.type_of param))
           in
           Hashtbl.add function_context.fctx_regs_map param smt_param;
           smt_param)
         fn.fn_params
  in

  let create_function_decl name return_type =
    Z3.FuncDecl.mk_rec_func_decl_s ctx.ctx_smt_ctx
      (fn.fn_name ^ "_" ^ name)
      params_types return_type
  in

  let boolean_sort = Z3.Boolean.mk_sort ctx.ctx_smt_ctx in
  let constraints_fn = create_function_decl "constraints" boolean_sort in
  let ubs_fn = create_function_decl "ubs" boolean_sort in
  let return_fn =
    create_function_decl "return" (encode_type ctx (Function.return_type_of fn))
  in

  let entry_block = Option.get (Function.entry_block fn) in
  let idom = Ir_cfg.Dominator.compute_idom fn entry_block in
  let dom_tree = Ir_cfg.Dominator.idom_to_dom_tree fn idom in

  let rec traverse_blocks mem block =
    let mem = encode_block function_context mem block in
    let children = dom_tree block in
    List.fold_left traverse_blocks mem children
  in
  let final_memory = traverse_blocks initial_mem entry_block in
  ignore final_memory;

  (* Generate constraints for alloca instructions. In particular, alloca addresses do not overlap. *)
  let allocas_constraints =
    fst
      (List.fold_left
         (fun (constraints, previous_end) (addr, size) ->
           let size_int =
             Z3.Arithmetic.Integer.mk_numeral_i ctx.ctx_smt_ctx size
           in
           let end_addr =
             Z3.Arithmetic.mk_add ctx.ctx_smt_ctx [ addr; size_int ]
           in
           ( Z3.Boolean.mk_eq ctx.ctx_smt_ctx addr previous_end :: constraints,
             end_addr ))
         ([], Z3.Arithmetic.Integer.mk_numeral_i ctx.ctx_smt_ctx 0)
         function_context.fctx_allocas)
  in

  let preds_constraints =
    Hashtbl.fold
      (fun label conds acc ->
        let block_reachable =
          Hashtbl.find function_context.fctx_reachable_blocks label
        in
        let preds_condition = Z3.Boolean.mk_or ctx.ctx_smt_ctx conds in
        let constraint_expr =
          Z3.Boolean.mk_implies ctx.ctx_smt_ctx block_reachable preds_condition
        in
        constraint_expr :: acc)
      function_context.fctx_preds_conditions []
  in

  Z3.FuncDecl.add_rec_def ctx.ctx_smt_ctx constraints_fn params_encoded
    (Z3.Boolean.mk_and ctx.ctx_smt_ctx
       (function_context.fctx_constraints @ allocas_constraints
      @ preds_constraints));

  Z3.FuncDecl.add_rec_def ctx.ctx_smt_ctx ubs_fn params_encoded
    (Z3.Boolean.mk_or ctx.ctx_smt_ctx function_context.fctx_ubs);

  let return_value =
    match function_context.fctx_return_value with
    | Some v -> v
    | None -> ctx.ctx_unit_const
  in
  Z3.FuncDecl.add_rec_def ctx.ctx_smt_ctx return_fn params_encoded return_value;
  Z3.Expr.to_string return_value
  |> Printf.printf "Return value for %s: %s\n" fn.fn_name;

  { constraints_fn; ubs_fn; return_fn }

(** Try to convert an SMT expression to an IR value. *)
let ir_value_from_smt_expr expr =
  if Z3.Boolean.is_bool expr then
    let value = Z3.Boolean.get_bool_value expr in
    match value with
    | Z3enums.L_TRUE -> Ival_int (Ityp_i1, Z.one)
    | Z3enums.L_FALSE -> Ival_int (Ityp_i1, Z.zero)
    | Z3enums.L_UNDEF -> failwith "Undefined boolean value in SMT expression"
  else if Z3.BitVector.is_bv expr then
    let bw = Z3.BitVector.get_size (Z3.Expr.get_sort expr) in
    let value_str = Z3.BitVector.numeral_to_string expr in
    let value = Z.of_string value_str in
    match bw with
    | 1 -> Ival_int (Ityp_i1, value)
    | 8 -> Ival_int (Ityp_i8, value)
    | 16 -> Ival_int (Ityp_i16, value)
    | 32 -> Ival_int (Ityp_i32, value)
    | 64 -> Ival_int (Ityp_i64, value)
    | _ -> failwith "Unsupported bitwidth for integer value"
  else failwith "Unsupported SMT expression for IR value"

let check_equivalence ctx source_fn target_fn =
  Z3.Solver.push ctx.ctx_smt_solver;

  (* The global memory state. *)
  let memory =
    Z3.Expr.mk_fresh_const ctx.ctx_smt_ctx "memory" ctx.ctx_mem_sort
  in

  (* Create fresh global constants for the variables. The first argument is a bit special,
     it is by convention the global memory state. *)
  let source_params_sorts = Z3.FuncDecl.get_domain source_fn.constraints_fn in
  let args =
    memory
    :: List.map
         (fun param_sort ->
           Z3.Expr.mk_fresh_const ctx.ctx_smt_ctx "arg" param_sort)
         (List.tl source_params_sorts)
  in

  let source_ubs = Z3.FuncDecl.apply source_fn.ubs_fn args in
  let target_ubs = Z3.FuncDecl.apply target_fn.ubs_fn args in
  let source_return = Z3.FuncDecl.apply source_fn.return_fn args in
  let target_return = Z3.FuncDecl.apply target_fn.return_fn args in

  Z3.Solver.add ctx.ctx_smt_solver
    [
      Z3.FuncDecl.apply source_fn.constraints_fn args;
      Z3.FuncDecl.apply target_fn.constraints_fn args;
      Z3.Boolean.mk_not ctx.ctx_smt_ctx
        (Z3.Boolean.mk_and ctx.ctx_smt_ctx
           [
             Z3.Boolean.mk_eq ctx.ctx_smt_ctx source_ubs target_ubs;
             Z3.Boolean.mk_eq ctx.ctx_smt_ctx source_return target_return;
           ]);
    ];

  let result =
    match Z3.Solver.check ctx.ctx_smt_solver [] with
    | Z3.Solver.UNSATISFIABLE -> Equivalent
    | Z3.Solver.UNKNOWN ->
        Unknown (Z3.Solver.get_reason_unknown ctx.ctx_smt_solver)
    | Z3.Solver.SATISFIABLE ->
        let model = Option.get (Z3.Solver.get_model ctx.ctx_smt_solver) in

        let args_vals =
          List.map (fun arg -> Z3.Model.eval model arg true) args
        in

        let memory_val = List.hd args_vals in
        let real_args_vals = List.tl args_vals in

        let source_ubs_val = Z3.Model.eval model source_ubs true in
        let target_ubs_val = Z3.Model.eval model target_ubs true in
        let source_return_val = Z3.Model.eval model source_return true in
        let target_return_val = Z3.Model.eval model target_return true in

        let get_bool_value_opt expr_opt =
          match expr_opt with
          | None -> false
          | Some v -> (
              match Z3.Boolean.get_bool_value v with
              | Z3enums.L_TRUE -> true
              | Z3enums.L_FALSE | Z3enums.L_UNDEF -> false)
        in

        let fail_info =
          {
            memory = Option.get memory_val;
            args = List.map (fun v -> Option.get v) real_args_vals;
            src_return = Option.get source_return_val;
            tgt_return = Option.get target_return_val;
            src_has_ub = get_bool_value_opt source_ubs_val;
            tgt_has_ub = get_bool_value_opt target_ubs_val;
          }
        in
        Not_equivalent fail_info
  in

  Z3.Solver.pop ctx.ctx_smt_solver 1;
  result

let check_module ir_ctx =
  let source_name = "source" in
  let target_name = "target" in

  let source_func = Ir.Module.lookup_function ir_ctx source_name in
  let target_func = Ir.Module.lookup_function ir_ctx target_name in

  if source_func.fn_type <> target_func.fn_type then
    failwith "Source and target functions have different types";

  let cfg = [ ("timeout", "10000") ] in
  let smt_ctx = Z3.mk_context cfg in
  let ctx = create_context smt_ctx in
  let source_encoded = encode_function ctx source_func in
  let target_encoded = encode_function ctx target_func in
  match check_equivalence ctx source_encoded target_encoded with
  | Equivalent -> Printf.printf "The functions are equivalent.\n"
  | Unknown reason -> Printf.printf "Equivalence check unknown: %s\n" reason
  | Not_equivalent info ->
      Printf.printf "The functions are not equivalent.\n";
      Printf.printf "Counter example:\n";
      Printf.printf "- Memory: %s\n" (Z3.Expr.to_string info.memory);
      List.iteri
        (fun i arg ->
          Format.printf "- Arg %d: %a\n" i Ir_printer.pp_value
            (ir_value_from_smt_expr arg))
        info.args;
      Format.printf "- Source return: %a\n" Ir_printer.pp_value
        (ir_value_from_smt_expr info.src_return);
      Format.printf "- Target return: %a\n" Ir_printer.pp_value
        (ir_value_from_smt_expr info.tgt_return);
      Printf.printf "- Source has UB: %b\n" info.src_has_ub;
      Printf.printf "- Target has UB: %b\n" info.tgt_has_ub
