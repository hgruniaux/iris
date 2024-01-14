(*
  The definition of the intermediate representation (IR) tree.
*)

(** A module to manipulate registers. *)
module Reg = struct
  type t = int

  let first_pseudo_reg = 128

  (** Returns true if [x] is a pseudo register; false otherwise. *)
  let is_pseudo (x : t) = x >= first_pseudo_reg

  let is_physical (x : t) = x >= 0 && x < first_pseudo_reg

  (** Creates a new fresh pseudo register *)
  let fresh =
    let cpt = ref first_pseudo_reg in
    fun () ->
      incr cpt;
      !cpt

  (** Creates a physical register with the given index [i]. *)
  let physical i =
    assert (i >= 0 && i < first_pseudo_reg);
    i

  let compare (x : t) (y : t) = Stdlib.compare x y

  module Map = Map.Make (Int)
  module Set = Set.Make (Int)

  type 'a map = 'a Map.t
  type set = Set.t
end

module Label = UniqueId
module Constant = UniqueId

type reg = Reg.t
type label = Label.t
type constant = Constant.t

type typ = Ityp_void | Ityp_int | Ityp_ptr | Ityp_func of typ * typ list
(*
  | Ityp_array of typ
  | Ityp_struct of typ list
  | Ityp_union of typ list
  *)

and ctx = {
  mutable ctx_funcs : fn list;
  ctx_symbol_table : (string, fn) Hashtbl.t;
  ctx_constants : (constant, constant_value) Hashtbl.t;
      (** The hash table that links constant IDs to their constant value. *)
  ctx_string_constants : (string, constant) Hashtbl.t;
  ctx_int_constants : (Z.t, constant) Hashtbl.t;
}

and constant_value =
  | Icst_int of Z.t
      (** A machine-dependant integer. All integers use 2-complement representation. *)
  | Icst_string of string
      (** A byte string. The string is not implicitly NUL-terminated. *)

and fn = {
  fn_name : string;
      (** The function's name. Some backends, linkers or assemblers have
          restrictions on what kind of name are accepted or how names are encoded. *)
  fn_type : typ;  (** The function's type. Must be an instance of Ityp_func. *)
  fn_params : reg list;
      (** The names of the function's parameters. There is the same count of
          parameters as specified in the function's type. *)
  fn_ctx : ctx;
      (** The Iris context to whom this function belongs. A function can not be
          shared among different contexts. *)
  mutable fn_blocks : bb Label.map;
  mutable fn_entry : label option;
  fn_symbol_table : (reg, inst) Hashtbl.t;
      (** The internal symbol table of the function. It maps a register/name to
          its associated instruction. *)
  fn_is_external : bool;
      (** True if this function is external. That is, if it is defined outside
          the scope of Iris (in another library or file). External functions do
          not have an implementation. *)
}

and bb = {
  b_func : fn;
  b_label : label;
  mutable b_phi_insts : inst list;
  mutable b_insts : inst list;
  mutable b_term : inst option;
  mutable b_predecessors : Label.set;
  mutable b_successors : Label.set;
}

and operand = Iop_reg of reg | Iop_imm of Z.t

and inst = {
  i_name : reg;
  mutable i_kind : inst_kind;
  i_typ : typ;
  mutable i_bb : bb;
  (* The instructions that are used by this instruction. *)
  mutable i_uses : inst list;
  (* Instructions are a doubly-linked list. *)
  mutable i_next : inst option;
  mutable i_prev : inst option;
}

and inst_kind =
  | Iinst_cst of constant
  | Iinst_loadi of Z.t
  | Iinst_mov of reg
  | Iinst_load of reg
  | Iinst_store of reg * operand
  | Iinst_alloca of typ
  | Iinst_binop of binop * operand * operand
  | Iinst_unop of unop * operand
  | Iinst_cmp of cmp * operand * operand
  | Iinst_call of fn * operand list
  | Iinst_phi of (operand * label) list
  (* TODO: move terminator instruction to their own type *)
  | Iinst_unreachable
  | Iinst_ret
  | Iinst_retv of operand
  | Iinst_jmp of label
  | Iinst_jmpc of operand * label * label

and binop =
  | Ibinop_add (* Integer addition *)
  | Ibinop_sub (* Integer subtraction *)
  | Ibinop_mul (* Integer multiplication *)
  | Ibinop_udiv (* Unsigned integer division *)
  | Ibinop_sdiv (* Signed integer division *)
  | Ibinop_urem (* Unsigned integer division remainder *)
  | Ibinop_srem (* Signed integer division remainder *)
  | Ibinop_lsl (* Logical left shift *)
  | Ibinop_asr (* Arithmetic right shift *)
  | Ibinop_lsr (* Logical right shift *)
  | Ibinop_and (* Bitwise and *)
  | Ibinop_or (* Bitwise or *)
  | Ibinop_xor (* Bitwise xor *)

and unop = Iunop_neg | Iunop_not

and cmp =
  | Icmp_eq (* Equal to *)
  | Icmp_ne (* Not equal to *)
  | Icmp_ult (* Unsigned less than *)
  | Icmp_ule (* Unsigned less than or equal to *)
  | Icmp_ugt (* Unsigned greater than *)
  | Icmp_uge (* Unsigned greater than or equal to *)
  | Icmp_slt (* Signed less than *)
  | Icmp_sle (* Signed less than or equal to *)
  | Icmp_sgt (* Signed greater than *)
  | Icmp_sge (* Signed greater than or equal to *)

(** Checks if [inst_a] and [inst_b] live in the same basic block. *)
let in_same_bb inst_a inst_b = inst_a.i_bb == inst_b.i_bb

(** Checks if [inst_kind] represents a PHI instruction. *)
let is_phi inst_kind = match inst_kind with Iinst_phi _ -> true | _ -> false

(** Checks if [inst_kind] represents a terminator instruction. *)
let is_term inst_kind =
  match inst_kind with
  | Iinst_unreachable | Iinst_ret | Iinst_retv _ | Iinst_jmp _ | Iinst_jmpc _ ->
      true
  | _ -> false

(** Returns the type of [reg] in the given [fn]. *)
let type_of_reg fn reg =
  match Hashtbl.find_opt fn.fn_symbol_table reg with
  | None -> Ityp_int
  | Some inst -> inst.i_typ

(** Returns the type of [operand] in the given [fn]. *)
let type_of_operand fn operand =
  match operand with Iop_imm _ -> Ityp_int | Iop_reg r -> type_of_reg fn r

let mk_ctx () =
  {
    ctx_symbol_table = Hashtbl.create 17;
    ctx_funcs = [];
    ctx_constants = Hashtbl.create 17;
    ctx_string_constants = Hashtbl.create 0;
    ctx_int_constants = Hashtbl.create 0;
  }

(** Returns the integer constant [i] of [ctx]. *)
let get_int_constant ctx i =
  match Hashtbl.find_opt ctx.ctx_int_constants i with
  | Some c -> c
  | None ->
      let id = Constant.fresh () in
      Hashtbl.add ctx.ctx_constants id (Icst_int i);
      Hashtbl.add ctx.ctx_int_constants i id;
      id

(** Returns the string constant [s] (not NUL-terminated) of [ctx]. *)
let get_string_constant ctx s =
  match Hashtbl.find_opt ctx.ctx_string_constants s with
  | Some c -> c
  | None ->
      let id = Constant.fresh () in
      Hashtbl.add ctx.ctx_constants id (Icst_string s);
      Hashtbl.add ctx.ctx_string_constants s id;
      id

(** Returns the function named [name] in the given [ctx]; or None if no such function. *)
let get_fn ctx name = Hashtbl.find_opt ctx.ctx_symbol_table name

(** Returns the function named [name] in the given [ctx].

    If such function does not exist, then a new function is created with
    the given [ret_ty] and [args_ty], and added to the [ctx].

    If the function already exists, it is checked if it has the
    expected type and attributes.

    If [is_external] is true, the function is marked as external (defined
    elsewhere). You can not add code to an external function. External
    functions include functions from other libraries (such as the C standard
    library). *)
let get_or_insert_fn ctx name ?(is_external = false) ret_ty args_ty =
  match get_fn ctx name with
  | Some fn ->
      assert (fn.fn_type = Ityp_func (ret_ty, args_ty));
      assert (fn.fn_is_external = is_external);
      fn
  | None ->
      let fn =
        {
          fn_name = name;
          fn_type = Ityp_func (ret_ty, args_ty);
          fn_params = List.init (List.length args_ty) (fun _ -> Reg.fresh ());
          fn_ctx = ctx;
          fn_symbol_table = Hashtbl.create (if is_external then 0 else 17);
          fn_blocks = Label.Map.empty;
          fn_entry = None;
          fn_is_external = is_external;
        }
      in
      Hashtbl.add ctx.ctx_symbol_table name fn;
      ctx.ctx_funcs <- fn :: ctx.ctx_funcs;
      fn

(** Returns the return's type of [fn]. *)
let return_type_of fn =
  match fn.fn_type with Ityp_func (ret_ty, _) -> ret_ty | _ -> assert false

(** Returns the parameter types of [fn]. *)
let param_types_of fn =
  match fn.fn_type with
  | Ityp_func (_, param_tys) -> param_tys
  | _ -> assert false

(** Creates a new basic block and adds it to [func]. *)
let mk_bb func =
  let label = Label.fresh () in
  let bb =
    {
      b_func = func;
      b_label = label;
      b_phi_insts = [];
      b_insts = [];
      b_term = None;
      b_predecessors = Label.Set.empty;
      b_successors = Label.Set.empty;
    }
  in
  func.fn_blocks <- Label.Map.add label bb func.fn_blocks;
  bb

let is_entry_bb bb =
  match bb.b_func.fn_entry with
  | None -> false
  | Some entry_label -> bb.b_label = entry_label

let update_uses inst ~remove =
  let fn = inst.i_bb.b_func in
  let update_uses_in_reg r =
    match Hashtbl.find_opt fn.fn_symbol_table r with
    | None -> ()
    | Some inst_to_update ->
        if remove then
          inst_to_update.i_uses <-
            List.filter (fun user -> user != inst) inst_to_update.i_uses
        else inst_to_update.i_uses <- inst :: inst_to_update.i_uses
  in
  let update_uses_in_op op =
    match op with Iop_imm _ -> () | Iop_reg r -> update_uses_in_reg r
  in
  match inst.i_kind with
  | Iinst_cst _ | Iinst_loadi _ | Iinst_alloca _ -> ()
  | Iinst_mov r | Iinst_load r -> update_uses_in_reg r
  | Iinst_store (r, o) ->
      update_uses_in_reg r;
      update_uses_in_op o
  | Iinst_binop (_, o1, o2) | Iinst_cmp (_, o1, o2) ->
      update_uses_in_op o1;
      update_uses_in_op o2
  | Iinst_unop (_, o) -> update_uses_in_op o
  | Iinst_call (_, args) -> List.iter update_uses_in_op args
  | Iinst_phi predecessors ->
      List.iter (fun (r, _) -> update_uses_in_op r) predecessors
  | Iinst_jmp _ | Iinst_ret | Iinst_unreachable -> ()
  | Iinst_retv o | Iinst_jmpc (o, _, _) -> update_uses_in_op o

exception Found_type of typ

let compute_inst_type bb kind =
  let type_of_reg r =
    match Hashtbl.find_opt bb.b_func.fn_symbol_table r with
    | Some inst -> inst.i_typ
    | None -> (
        (* Maybe the register names a parameter and not an instruction. *)
        try
          List.iter2
            (fun param typ -> if param = r then raise (Found_type typ))
            bb.b_func.fn_params (param_types_of bb.b_func);

          (* Should not happen. *)
          assert false
        with Found_type t -> t)
  in
  let type_of_operand = function
    | Iop_imm _ -> Ityp_int
    | Iop_reg r -> type_of_reg r
  in

  match kind with
  | Iinst_alloca _ -> Ityp_ptr
  | Iinst_mov r -> type_of_reg r
  | Iinst_load addr ->
      assert (type_of_reg addr = Ityp_ptr);
      Ityp_int (* TODO *)
  | Iinst_cst cst -> (
      match Hashtbl.find_opt bb.b_func.fn_ctx.ctx_constants cst with
      | Some (Icst_int _) -> Ityp_int
      | Some (Icst_string _) -> Ityp_ptr
      | None -> assert false)
  | Iinst_loadi _ -> Ityp_int
  | Iinst_binop (_, lhs, rhs) | Iinst_cmp (_, lhs, rhs) ->
      let lhs_t = type_of_operand lhs in
      let rhs_t = type_of_operand rhs in
      assert (lhs_t = Ityp_int && lhs_t = rhs_t);
      lhs_t
  | Iinst_unop (_, exp) ->
      let exp_t = type_of_operand exp in
      assert (exp_t = Ityp_int);
      exp_t
  | Iinst_store (addr, _) ->
      assert (type_of_reg addr = Ityp_ptr);
      Ityp_void
  | Iinst_call (fn, args) ->
      (* Check if the argument types are correct. *)
      List.iter2
        (fun arg expected_typ -> assert (type_of_operand arg = expected_typ))
        args (param_types_of fn);
      return_type_of fn
  | Iinst_phi predecessors ->
      Option.get
        (List.fold_left
           (fun typ (operand, _) ->
             let new_typ = type_of_operand operand in
             match typ with
             | None -> Some new_typ
             | Some typ ->
                 assert (typ = new_typ);
                 Some new_typ)
           None predecessors)
      (* Terminator instructions. *)
  | Iinst_unreachable | Iinst_ret | Iinst_retv _ | Iinst_jmp _ | Iinst_jmpc _ ->
      Ityp_void

(** Creates an instruction of the given [kind] and [name] inside [bb].
    However, the instruction is not yet inserted in one of [bb]'s instruction list. *)
let mk_inst bb name kind =
  let inst =
    {
      i_name = name;
      i_kind = kind;
      i_typ = compute_inst_type bb kind;
      i_bb = bb;
      i_uses = [];
      i_prev = None;
      i_next = None;
    }
  in
  Hashtbl.add bb.b_func.fn_symbol_table name inst;
  (* Find each instruction used by [inst] and update their i_uses field. *)
  update_uses inst ~remove:false;
  inst

(** Checks if [inst] has side effects. *)
let has_sideeffect inst =
  match inst.i_kind with
  | Iinst_cst _ | Iinst_loadi _ | Iinst_mov _ | Iinst_load _ | Iinst_phi _
  | Iinst_binop _ | Iinst_unop _ | Iinst_cmp _ ->
      false
  | _ -> true

(** Inserts a phi node with the given [operands] into the given [bb]. *)
let insert_phi bb operands =
  assert (operands <> []);
  let name = Reg.fresh () in
  let inst = mk_inst bb name (Iinst_phi operands) in
  (* Insert the PHI node in [bb]. *)
  bb.b_phi_insts <- inst :: bb.b_phi_insts;
  name

(** Returns the successors of a given terminator instruction kind. *)
let successors_of_term = function
  | Iinst_unreachable | Iinst_ret | Iinst_retv _ -> Label.Set.empty
  | Iinst_jmp bb -> Label.Set.singleton bb
  | Iinst_jmpc (_, then_bb, else_bb) -> Label.Set.of_list [ then_bb; else_bb ]
  | _ -> failwith "not a terminator instruction"

(** Sets the [bb]'s terminator to an instruction of the given [kind]. *)
let set_term bb kind =
  assert (is_term kind);

  (match bb.b_term with
  | None -> ()
  | Some inst -> Hashtbl.remove bb.b_func.fn_symbol_table inst.i_name);

  let name = Reg.fresh () in
  let inst = mk_inst bb name kind in
  bb.b_term <- Some inst;

  (* Update successors. *)

  (* First we remove [bb] from its successors' predecessors list. *)
  Label.Set.iter
    (fun succ_label ->
      let succ = Label.Map.find succ_label bb.b_func.fn_blocks in
      succ.b_predecessors <- Label.Set.remove bb.b_label succ.b_predecessors)
    bb.b_successors;

  (* Then we add [bb] to its new successors. *)
  bb.b_successors <- successors_of_term kind;
  Label.Set.iter
    (fun succ_label ->
      let succ = Label.Map.find succ_label bb.b_func.fn_blocks in
      succ.b_predecessors <- Label.Set.add bb.b_label succ.b_predecessors)
    bb.b_successors

let iter_insts f bb =
  (* First, iterate over PHI nodes. *)
  List.iter f bb.b_phi_insts;
  List.iter f bb.b_insts;
  (* And finally, handle the terminator. *)
  match bb.b_term with None -> () | Some term -> f term

let fold_insts f init bb =
  let value = ref init in
  iter_insts (fun inst -> value := f !value inst) bb;
  !value
