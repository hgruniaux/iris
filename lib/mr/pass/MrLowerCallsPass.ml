open Mr

(** Returns the first [k] elements of [xs] and the remaining elements.
    If [xs] is too small, then [xs] is returned. *)
let rec firstk k xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
      if k = 0 then ([], x :: xs)
      else if k = 1 then ([ x ], xs)
      else
        let f, r = firstk (k - 1) xs in
        (x :: f, r)

(** Same as List.map2 but stop when l1 is empty (therefore both lists may
        have a different length). *)
let rec map2 f l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> failwith "not enough elements in l2"
  | x1 :: r1, x2 :: r2 -> f x1 x2 :: map2 f r1 r2

(** This pass lower call instructions by expliciting the calling convention.
    In other words, it inserts move and push instructions to pass the
    arguments, etc. *)
let pass_fn arch fn =
  let cc_info = Backend.cc_info arch in
  MirPassUtils.map_insts fn (fun inst ->
      if inst.mi_kind <> "call" then [ inst ]
      else
        let return_reg =
          match List.hd inst.mi_operands with
          | Oreg r -> r
          | _ ->
              failwith
                "expected a register as the first operand of a call instruction"
        in
        let callee =
          match List.hd (List.tl inst.mi_operands) with
          | Ofunc f -> f
          | _ ->
              failwith
                "expected a function as the second operand of a call \
                 instruction"
        in
        let args = List.tl (List.tl inst.mi_operands) in

        let args_in_regs, args_in_stack =
          firstk cc_info.cc_args_regs_count args
        in

        (* Inserts the move instructions for the parameters lying in registers. *)
        let mov_insts =
          map2
            (fun arg reg -> MirBuilder.mk_mov_operand arch reg arg)
            args_in_regs cc_info.cc_args_regs
        in

        (* Push instructions for the parameters lying in the stack. *)
        let push_insts =
          List.fold_right
            (fun operand insts ->
              MirBuilder.mk_push_operand arch operand :: insts)
            args_in_stack []
        in

        (* Reverse push instructions if arguments are expected left to right. *)
        let push_insts =
          if cc_info.cc_args_stack_ltr then List.rev push_insts else push_insts
        in

        let precall_insts = mov_insts @ push_insts in

        let reg_defs = ref (Reg.Set.elements cc_info.cc_caller_saved) in

        let postcall_insts =
          (* Pop arguments from stack if required. *)
          (if cc_info.cc_caller_cleanup then
             MirBuilder.mk_pop_many arch (List.length args_in_stack)
           else [])
          @
          if Ir.return_type_of callee <> Ir.Ityp_void then
            (* Retrieve the returned value either from a physical register
               or the stack depending on the calling convention. *)
            match cc_info.cc_return_reg with
            | Some r ->
                reg_defs := return_reg :: !reg_defs;
                [ MirBuilder.mk_mov arch return_reg r ]
            | None -> [ MirBuilder.mk_pop arch return_reg ]
          else []
        in

        precall_insts
        @ Mr.mk_inst "call" [ Ofunc callee ] ~defs:!reg_defs ~uses:[]
          :: postcall_insts)
