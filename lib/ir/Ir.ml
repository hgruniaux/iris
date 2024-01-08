(*
  The definition of the intermediate representation (IR) tree.
*)

module type Id = sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

  type 'a map = 'a Map.t
  type set = Set.t
end

(** A module to manipulate unique ids. *)
module UniqueId : Id = struct
  type t = int

  let fresh =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt

  let compare (x : t) (y : t) = Stdlib.compare x y

  module Map = Map.Make (Int)
  module Set = Set.Make (Int)

  type 'a map = 'a Map.t
  type set = Set.t
end

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

module Label : Id = UniqueId

type reg = Reg.t
type label = Label.t

type typ = Ityp_void | Ityp_int | Ityp_ptr

and ctx = {
  mutable ctx_funcs : fn list;
  ctx_symbol_table : (string, fn) Hashtbl.t;
}

and fn = {
  fn_name : string;
  fn_params : reg list;
  fn_ctx : ctx;
  mutable fn_blocks : bb Label.map;
  mutable fn_entry : label option;
  fn_symbol_table : (reg, inst) Hashtbl.t;
}

and bb = {
  b_func : fn;
  b_label : label;
  mutable b_first_phi : inst option;
  mutable b_first_inst : inst option;
  mutable b_last_inst : inst option;
  mutable b_term : inst option;
  mutable b_predecessors : Label.set;
  mutable b_successors : Label.set;
}

and operand = Iop_reg of reg | Iop_imm of nativeint

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
  | Iinst_cst of nativeint
  | Iinst_mov of reg
  | Iinst_load of reg
  | Iinst_store of reg * operand
  | Iinst_alloca of typ
  | Iinst_binop of binop * operand * operand
  | Iinst_unop of unop * operand
  | Iinst_cmp of cmp * operand * operand
  | Iinst_call of fn * operand list
  | Iinst_phi of (operand * label) list
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
  | Iinst_ret | Iinst_retv _ | Iinst_jmp _ | Iinst_jmpc _ -> true
  | _ -> false

(** Returns the type of [reg] in the given [fn]. *)
let type_of_reg fn reg =
  match Hashtbl.find_opt fn.fn_symbol_table reg with
  | None -> Ityp_int
  | Some inst -> inst.i_typ

(** Returns the type of [operand] in the given [fn]. *)
let type_of_operand fn operand =
  match operand with Iop_imm _ -> Ityp_int | Iop_reg r -> type_of_reg fn r

let mk_ctx () = { ctx_symbol_table = Hashtbl.create 17; ctx_funcs = [] }

let mk_fn ctx name arity =
  let fn =
    {
      fn_name = name;
      fn_params = List.init arity (fun _ -> Reg.fresh ());
      fn_ctx = ctx;
      fn_symbol_table = Hashtbl.create 17;
      fn_blocks = Label.Map.empty;
      fn_entry = None;
    }
  in
  Hashtbl.add ctx.ctx_symbol_table name fn;
  ctx.ctx_funcs <- fn :: ctx.ctx_funcs;
  fn

(** Creates a new basic block and adds it to [func]. *)
let mk_bb func =
  let label = Label.fresh () in
  let bb =
    {
      b_func = func;
      b_label = label;
      b_first_phi = None;
      b_first_inst = None;
      b_last_inst = None;
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
  | Iinst_cst _ | Iinst_ret | Iinst_jmp _ | Iinst_alloca _ -> ()
  | Iinst_mov r | Iinst_load r -> update_uses_in_reg r
  | Iinst_store (r, o) ->
      update_uses_in_reg r;
      update_uses_in_op o
  | Iinst_binop (_, o1, o2) | Iinst_cmp (_, o1, o2) ->
      update_uses_in_op o1;
      update_uses_in_op o2
  | Iinst_unop (_, o) | Iinst_retv o | Iinst_jmpc (o, _, _) ->
      update_uses_in_op o
  | Iinst_call (_, args) -> List.iter update_uses_in_op args
  | Iinst_phi predecessors ->
      List.iter (fun (r, _) -> update_uses_in_op r) predecessors

let compute_inst_type bb kind =
  match kind with
  | Iinst_alloca _ -> Ityp_ptr
  | Iinst_mov r -> type_of_reg bb.b_func r
  | Iinst_load _ -> Ityp_int (* TODO *)
  | Iinst_cst _ | Iinst_binop _ | Iinst_unop _ | Iinst_cmp _ -> Ityp_int
  | Iinst_store _ | Iinst_ret | Iinst_retv _ | Iinst_jmp _ | Iinst_jmpc _ ->
      Ityp_void
  | Iinst_call _ -> Ityp_int (* TODO *)
  | Iinst_phi predecessors ->
      Option.get
        (List.fold_left
           (fun typ (operand, _) ->
             let new_typ = type_of_operand bb.b_func operand in
             match typ with
             | None -> Some new_typ
             | Some typ ->
                 assert (typ = new_typ);
                 Some new_typ)
           None predecessors)

(** Creates an instruction of the given [typ], [kind] and [name] inside [bb].
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
  | Iinst_cst _ | Iinst_mov _ | Iinst_load _ | Iinst_phi _ | Iinst_binop _
  | Iinst_unop _ | Iinst_cmp _ ->
      false
  | _ -> true

(** Inserts the instruction [new_inst] before the given instruction [before_inst].
    Both instructions must live in the same basic block. *)
let insert_before before_inst new_inst =
  assert (in_same_bb before_inst new_inst);
  assert (not (is_phi new_inst.i_kind || is_term new_inst.i_kind));
  (* Update the links. *)
  new_inst.i_prev <- before_inst.i_prev;
  Option.iter (fun prev -> prev.i_next <- Some new_inst) before_inst.i_prev;
  before_inst.i_prev <- Some new_inst;
  new_inst.i_next <- Some before_inst;
  (* Update the head and tail. *)
  if Option.is_none new_inst.i_prev then
    new_inst.i_bb.b_first_inst <- Some new_inst

(** Inserts [inst] at start of [bb]'s instruction list (but after PHI nodes). *)
let insert_at_start bb inst =
  assert (inst.i_bb == bb);
  match bb.b_first_inst with
  | None ->
      bb.b_first_inst <- Some inst;
      bb.b_last_inst <- Some inst
  | Some first_inst -> insert_before first_inst inst

(** Inserts the instruction [new_inst] after the given instruction [after_inst].
    Both instructions must live in the same basic block. *)
let insert_after after_inst new_inst =
  assert (in_same_bb after_inst new_inst);
  assert (not (is_phi new_inst.i_kind || is_term new_inst.i_kind));
  (* Update the links. *)
  new_inst.i_next <- after_inst.i_next;
  Option.iter (fun next -> next.i_prev <- Some new_inst) after_inst.i_next;
  new_inst.i_prev <- Some after_inst;
  after_inst.i_next <- Some new_inst;
  (* Update the head and tail. *)
  if Option.is_none new_inst.i_next then
    new_inst.i_bb.b_last_inst <- Some new_inst

(** Inserts [inst] at end of [bb]'s instruction list (but before terminator). *)
let insert_at_end bb inst =
  assert (inst.i_bb == bb);
  match bb.b_last_inst with
  | None ->
      bb.b_first_inst <- Some inst;
      bb.b_last_inst <- Some inst
  | Some last_inst -> insert_after last_inst inst

(** Removes the given [inst] from its basic block and its parent's function. *)
let remove inst =
  assert (inst.i_uses = []);

  (* Update the head and tail of the list. *)
  let bb = inst.i_bb in
  (* This is a bit tricky because we have many lists with different kinds of instruction. *)
  (* A PHI instruction. *)
  if Option.fold ~none:false ~some:(fun i -> i == inst) bb.b_first_phi then
    bb.b_first_phi <- inst.i_next;
  (* A simple instruction. *)
  if Option.fold ~none:false ~some:(fun i -> i == inst) bb.b_first_inst then
    bb.b_first_inst <- inst.i_next;
  if Option.fold ~none:false ~some:(fun i -> i == inst) bb.b_last_inst then
    bb.b_last_inst <- inst.i_prev;
  (* A terminator. *)
  if Option.fold ~none:false ~some:(fun i -> i == inst) bb.b_term then
    bb.b_term <- None;

  (* Update the doubly-linked list. *)
  Option.iter (fun next -> next.i_prev <- inst.i_prev) inst.i_next;
  Option.iter (fun prev -> prev.i_next <- inst.i_next) inst.i_prev;
  inst.i_next <- None;
  inst.i_prev <- None;

  (* Remove from the function's symbol table. *)
  Hashtbl.remove bb.b_func.fn_symbol_table inst.i_name;

  update_uses inst ~remove:true

(** Same as [remove inst] but start by removing [inst]'s users before.
    Therefore, unlike [remove] this function does not need [inst] to have
    no users. *)
let rec remove_inst_and_users inst =
  List.iter (fun user -> remove_inst_and_users user) inst.i_uses;
  inst.i_uses <- [];
  remove inst

(** Inserts a phi node with the given [operands] into the given [bb]. *)
let insert_phi bb operands =
  assert (operands <> []);
  let name = Reg.fresh () in
  let inst = mk_inst bb name (Iinst_phi operands) in
  (* Insert the PHI node in [bb]. *)
  (match bb.b_first_phi with
  | None -> bb.b_first_phi <- Some inst
  | Some first_phi ->
      insert_before first_phi inst;
      bb.b_first_phi <- Some inst);
  name

(** Returns the successors of a given terminator instruction kind. *)
let successors_of_term = function
  | Iinst_ret | Iinst_retv _ -> Label.Set.empty
  | Iinst_jmp bb -> Label.Set.singleton bb
  | Iinst_jmpc (_, then_bb, else_bb) -> Label.Set.of_list [ then_bb; else_bb ]
  | _ -> failwith "not a terminator instruction"

(** Sets the [bb]'s terminator to an instruction of the given [kind]. *)
let set_term bb kind =
  assert (is_term kind);
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

let iter_phis f bb =
  let it = ref bb.b_first_phi in
  while Option.is_some !it do
    let inst = Option.get !it in
    f inst;
    it := inst.i_next
  done

let iter_insts f bb =
  (* First, iterate over PHI nodes. *)
  iter_phis f bb;

  (* Then, over basic instructions. *)
  let it = ref bb.b_first_inst in
  while Option.is_some !it do
    let inst = Option.get !it in
    f inst;
    it := inst.i_next
  done;

  (* And finally, handle the terminator. *)
  match bb.b_term with None -> () | Some term -> f term

let fold_insts f init bb =
  let value = ref init in
  iter_insts (fun inst -> value := f !value inst) bb;
  !value
