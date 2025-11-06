(*
  The definition of the intermediate representation (IR) tree.
*)

type typ =
  | Ityp_unit
  | Ityp_i1
  | Ityp_i8
  | Ityp_i16
  | Ityp_i32
  | Ityp_i64
  | Ityp_f32
  | Ityp_f64
  | Ityp_ptr
  | Ityp_array of typ * int
  | Ityp_func of typ list * typ * bool (* is_variadic *)
  | Ityp_struct of typ list

module Type = struct
  type t = typ

  (** Returns true if [typ] is a unit type. *)
  let is_unit typ = match typ with Ityp_unit -> true | _ -> false

  (** Returns true if [typ] is an integer type. *)
  let is_integer typ =
    match typ with
    | Ityp_ptr -> true (* Pointer types are considered integer types. *)
    | Ityp_i1 | Ityp_i8 | Ityp_i16 | Ityp_i32 | Ityp_i64 -> true
    | _ -> false

  (** Returns true if [typ] is a floating-point type. *)
  let is_float typ = match typ with Ityp_f32 | Ityp_f64 -> true | _ -> false

  let is_function typ = match typ with Ityp_func _ -> true | _ -> false

  (** The type of integer pointers. *)
  let integer_pointer_type = Ityp_i64

  let rec bitwidth = function
    | Ityp_unit -> 0
    | Ityp_i1 -> 1
    | Ityp_i8 -> 8
    | Ityp_i16 -> 16
    | Ityp_i32 -> 32
    | Ityp_i64 -> 64
    | Ityp_ptr -> bitwidth integer_pointer_type
    | Ityp_f32 -> 32
    | Ityp_f64 -> 64
    | _ -> failwith "Type.bitwidth: Not an integer or float type"
end

module Label = struct
  type t = { id : int; mutable name : string option }

  let create ?(name = None) id = { id; name }
  let equal l1 l2 = l1.id = l2.id
  let compare l1 l2 = Stdlib.compare l1.id l2.id
  let hash l = Hashtbl.hash l.id
  let name l = l.name
  let set_name l name = l.name <- Some name

  let pp_print fmt l =
    match l.name with
    | Some name -> Format.fprintf fmt "$%s.%d" name l.id
    | None -> Format.fprintf fmt "$%d" l.id
end

module LabelMap = Map.Make (Label)
module LabelSet = Set.Make (Label)

(** A module to manipulate registers. *)
module Reg = struct
  type t = { id : int; typ : typ; mutable name : string option }

  let type_of t = t.typ
  let first_pseudo_reg = 128
  let set_name reg name = reg.name <- Some name

  (** Returns true if [x] is a pseudo register; false otherwise. *)
  let is_pseudo (x : t) = x.id >= first_pseudo_reg

  let is_physical (x : t) = x.id >= 0 && x.id < first_pseudo_reg

  (** Creates a new fresh pseudo register *)
  let fresh =
    let cpt = ref first_pseudo_reg in
    fun typ ->
      incr cpt;
      { id = !cpt; typ; name = None }

  (* FIXME: Remove *)
  let create ?(name = None) id typ = { id = id + first_pseudo_reg; typ; name }

  (** Creates a physical register with the given index [i]. *)
  let physical i =
    assert (i >= 0 && i < first_pseudo_reg);
    { id = i; typ = Ityp_i64; name = None }

  let compare t1 t2 = Stdlib.compare t1.id t2.id
  let equal t1 t2 = t1.id = t2.id
  let hash t = Hashtbl.hash t.id
  let id t = t.id

  let pp_print fmt t =
    if is_physical t then Format.fprintf fmt "r%d" t.id
    else
      match t.name with
      | Some name -> Format.fprintf fmt "%%%s.%d" name (t.id - first_pseudo_reg)
      | None -> Format.fprintf fmt "%%%d" (t.id - first_pseudo_reg)
end

module RegMap = Map.Make (Reg)
module RegSet = Set.Make (Reg)
module Global = UniqueId

type reg = Reg.t
type label = Label.t
type constant = Global.t

(** Integer binary operators. *)
type ibinop =
  | Ibinop_add  (** Integer addition. *)
  | Ibinop_sub  (** Integer subtraction. *)
  | Ibinop_mul  (** Integer multiplication. *)
  | Ibinop_div_u  (** Unsigned integer division. *)
  | Ibinop_div_s  (** Signed integer division. *)
  | Ibinop_rem_u  (** Unsigned integer division. remainder. *)
  | Ibinop_rem_s  (** Signed integer division remainder. *)
  | Ibinop_lsl  (** Logical left shift. *)
  | Ibinop_asr  (** Arithmetic right shift. *)
  | Ibinop_lsr  (** Logical right shift. *)
  | Ibinop_and  (** Bitwise AND. *)
  | Ibinop_or  (** Bitwise OR. *)
  | Ibinop_xor  (** Bitwise XOR. *)

(** Integer unary operators. *)
type iunop =
  | Iunop_neg  (** Two's complement integer negation. *)
  | Iunop_not  (** Bitwise NOT. *)

(** Integer comparison operators. *)
type icmpop =
  | Icmp_eq  (** Equal to. *)
  | Icmp_ne  (** Not equal to. *)
  | Icmp_lt_u  (** Unsigned less than. *)
  | Icmp_le_u  (** Unsigned less than or equal to. *)
  | Icmp_gt_u  (** Unsigned greater than. *)
  | Icmp_ge_u  (** Unsigned greater than or equal to. *)
  | Icmp_lt_s  (** Signed less than. *)
  | Icmp_le_s  (** Signed less than or equal to. *)
  | Icmp_gt_s  (** Signed greater than. *)
  | Icmp_ge_s  (** Signed greater than or equal to. *)

type castop =
  | Icast_extend_s  (** Sign-extend an integer to a larger integer type. *)
  | Icast_extend_u  (** Zero-extend an integer to a larger integer type. *)
  | Icast_trunc  (** Truncate an integer to a smaller integer type. *)
  | Icast_promote  (** Promote a float to a larger float type. *)
  | Icast_demote  (** Demote a float to a smaller float type. *)
  | Icast_fp2ui  (** Cast a float to an unsigned integer type. *)
  | Icast_fp2si  (** Cast a float to a signed integer type. *)
  | Icast_ui2fp  (** Cast an unsigned integer to a float type. *)
  | Icast_si2fp  (** Cast a signed integer to a float type. *)
  | Icast_ptr2int  (** Cast a pointer to an integer type. *)
  | Icast_int2ptr  (** Cast an integer to a pointer type. *)
  | Icast_bitcast  (** Reinterpret the bits of a value as another type. *)

type value =
  | Ival_reg of Reg.t
  | Ival_global of global
  | Ival_int of typ * Z.t
  | Ival_float of typ * float

and ctx = {
  mod_symbol_table : (string, global) Hashtbl.t;
      (** The module's symbol table. All globals may not be added to the symbol
          table (e.g. private functions and variables). *)
  mutable mod_globals : global list;
      (** All the module's globals. This includes functions and global
          variables. *)
  mutable mod_global_cpt : int;
      (** A counter to generate unique unnamed global names. *)
}

and fn = {
  fn_name : string;
      (** The function's name. Some backends, linkers or assemblers have
          restrictions on what kind of name are accepted or how names are
          encoded. *)
  fn_type : typ;  (** The function's type. Must be an instance of Ityp_func. *)
  fn_params : reg list;
      (** The names of the function's parameters. There is the same count of
          parameters as specified in the function's type. *)
  fn_ctx : ctx;
      (** The Iris context to whom this function belongs. A function can not be
          shared among different contexts. *)
  mutable fn_blocks : bb LabelMap.t;
  mutable fn_entry : label option;
  fn_symbol_table : (reg, inst) Hashtbl.t;
      (** The internal symbol table of the function. It maps a register/name to
          its associated instruction. *)
  fn_is_external : bool;
      (** True if this function is external. That is, if it is defined outside
          the scope of Iris (in another library or file). External functions do
          not have an implementation. *)
  mutable fn_register_cpt : int;
      (** A counter to generate unique register ids. *)
  mutable fn_label_cpt : int;  (** A counter to generate unique label ids. *)
}

and bb = {
  block_label : label;
      (** The unique identifier of the basic block (per function). *)
  mutable block_args : reg list;  (** Arguments that this basic block takes. *)
  mutable b_phi_insts : inst list;
  mutable b_insts : inst list;
  mutable block_term : terminator;
      (** The terminator instruction of this basic block. *)
  mutable block_pred : LabelSet.t;
      (** The set of basic blocks labels that may jump to this basic block. *)
  mutable block_succ : LabelSet.t;
      (** The set of basic blocks labels that may be jumped to from this basic
          block. *)
}

and global = {
  global_id : Global.t;
      (** The unique identifier of the global (per module). *)
  global_name : string option;
      (** The global name. If the global is external, it will be used as the
          symbol name. The name may be None for internal unnamed globals (like
          string literals). *)
  global_mutable : bool;
      (** True if the global variable is mutable (i.e., can be modified at
          runtime). *)
  global_kind : global_kind;
}
(** Defines a global object. Functions, global variables, string literals, and
    others are considered as global objects. *)

and global_kind =
  | Iglobal_variable of typ * constant_value
      (** A global variable, optionally initialized with the given constant
          value. *)
  | Iglobal_function of fn
      (** A function which may be internal, external, etc. *)

and constant_value =
  | Iconstant_uninitialized  (** The value is uninitialized. *)
  | Iconstant_bytes of Bytes.t  (** The value is a byte sequence. *)
  | Iconstant_int of Z.t
      (** The value is an integer, the exact binary representation that will
          emitted depends on the global variable type. *)
  | Iconstant_float of float
      (** The value is a floating-point number, the exact binary representation
          that will be emitted depends on the global variable type. *)

and 'a generic_inst = { i_name : reg; mutable i_kind : 'a }
and inst = expression generic_inst

(** An expression used to defined a register. *)
and expression =
  | Iinst_value of value  (** [Iinst_value value] creates a copy of [value]. *)
  | Iinst_alloca of typ * int
      (** [Iinst_alloca allocated_type alignment] allocates enough space on the
          stack for the given [allocated_type] with at least the specified
          [alignment]. *)
  | Iinst_load of typ * value
      (** [Iinst_load loaded_type addr] loads a value of the given [loaded_type]
          from the memory address [addr]. *)
  | Iinst_store of value * value  (** [Iinst_store addr value]. *)
  | Iinst_cast of castop * typ * value
      (** [Iinst_cast op target_type value] casts [value] to [target_type] using
          the specified [op]. It is assumed that the cast is well-defined. *)
  | Iinst_ibinop of ibinop * value * value
      (** [Iinst_ibinop op lhs rhs] performs an integer binary operation. *)
  | Iinst_iunop of iunop * value
      (** [Iinst_iunop op value] performs an integer unary operation. *)
  | Iinst_icmp of icmpop * value * value
      (** [Iinst_icmp op lhs rhs] performs an integer comparison between [lhs]
          and [rhs]. The result is a boolean. *)
  | Iinst_call of value * value list
      (** [Iinst_call callee args] calls the function [callee] with the given
          [args]. The list of arguments must match the function's signature. *)
  | Iinst_phi of (value * Label.t) list

(** A instruction that execute some code in a basic block. Either it defines a
    register by evaluating an expression, or it stores a value into memory. *)
and instruction =
  | Istmt_def of reg * expression
      (** [Istmt_def name expr] defines the register/name [name] with the given
          [expr]. The IR must be in SSA form, so [name] can not be defined more
          than once, and its definition must dominates all of its uses. *)
  | Istmt_store of value * value
      (** [Istmt_store addr value] stores the given [value] at the memory
          address [addr]. *)

(** Terminator instructions. A terminator instruction is an instruction that
    ends a basic block. *)
and terminator =
  | Iterm_unreachable
      (** [Iterm_unreachable] asserts that this point is unreachable. *)
  | Iterm_ret of value option
      (** [Iterm_ret value_opt] returns from the function with the provided
          value [value_opt]. For a function with unit return type, [value_opt]
          can be None. *)
  | Iterm_br of Label.t * value list
      (** [Iterm_br target_label] jumps unconditionally to the [target_label]
          basic block. *)
  | Iterm_br_if of value * Label.t * value list * Label.t * value list
      (** [Iterm_br_if cond true_label true_args false_label false_args] jumps
          to [true_label] with [true_args] if [value] evaluates to true, or to
          [false_label] with [false_args] otherwise. *)
  | Iterm_br_table of
      value * label * value list * (Z.t * label * value list) list
      (** [Iterm_br_table cond default_label default_args cases] performs a jump
          table lookup on the given [cond] value. If [cond] matches one of the
          cases, it jumps to the corresponding label with the provided
          arguments. Otherwise, it jumps to the [default_label] with
          [default_args]. *)

let is_bb_from bb fn = LabelMap.mem bb.block_label fn.fn_blocks

let is_entry_bb fn bb =
  match fn.fn_entry with
  | None -> false
  | Some bb_label -> bb.block_label = bb_label
