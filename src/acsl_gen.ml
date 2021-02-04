(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Invariant_utils
open Cil_datatype

let dkey_acsl  = Mat_option.register_category "acsl_gen:acsl"
let dkey_term  = Mat_option.register_category "acsl_gen:term"
let dkey_zterm = Mat_option.register_category "acsl_gen:zterm"
let dkey_zero  = Mat_option.register_category "acsl_gen:iszero"

module Var_cpt = State_builder.SharedCounter(struct let name = "pilat_counter" end)

let emit_annot_list annots stmt kf =
  List.iter (fun annot ->
      let () =
        Mat_option.debug ~dkey:dkey_acsl
          "Annotation emited: %a on statement %a"
          Printer.pp_code_annotation annot
          Printer.pp_stmt stmt
      in
      let () = Annotations.add_code_annot Mat_option.emitter ~kf stmt annot
      in
      let ip = Property.ip_of_code_annot_single kf stmt annot in
      Property_status.emit Mat_option.emitter ~hyps:[] ip Property_status.True
    ) annots

let new_name () = Mat_option.NameConst.get () ^ (string_of_int (Var_cpt.next ()))

module Make(A:Poly_assign.S with type P.v = Varinfo.t)
    (C2A: sig val export_variables : unit -> Cil_types.varinfo A.P.Monom.Map.t end) =
struct

  module Invar_utils = Invariant_utils.Make(A)

  let variables_to_add = ref []
  let to_code_annot preds =
    List.fold_left
      (fun acc (pred,init_opt) ->
         let new_annot =
           Logic_const.new_code_annotation (AInvariant ([],true,pred))
         in
         let new_init =
           match init_opt with None -> snd acc
                             | Some s -> s :: snd acc
         in
         ((new_annot :: fst acc), new_init)
      )
      ([],[])
      preds

  let term_is_zero t =

    Mat_option.debug ~dkey:dkey_zero  "Testing %a"
      Printer.pp_term t ;
    match t.term_node with
    | TConst (Integer (i,_)) -> i = Integer.zero
    | TConst (LReal lr) -> lr.r_nearest = 0.
    | _ -> false

  let monomial_to_term (m:A.P.Monom.t) =

    let rec __m_to_term vars =
      match vars with
        [] -> Logic_const.term (TConst (Integer (Integer.one,(Some "1")))) Linteger
      | var :: [] ->
        let lvar = Cil.cvar_to_lvar var in
        let term =
          Logic_const.term
            (TLval
               (TVar lvar,TNoOffset)
            )
            Linteger
        in
        Mat_option.debug ~dkey:dkey_term ~level:3
          "End of the var list. Partial term generated : %a"
          Printer.pp_term term ;
        term

      | var :: tl ->
        let lvar = Cil.cvar_to_lvar var in
        let tlval = Logic_const.term (TLval (TVar lvar,TNoOffset)) Linteger in
        let end_term =  __m_to_term tl in
        let res =
          if term_is_zero end_term
          then Logic_const.term (TConst (Integer (Integer.zero,(Some "0")))) Linteger
          else

            Logic_const.term (TBinOp (Mult,tlval,end_term)) Linteger
        in
        Mat_option.debug ~dkey:dkey_term ~level:3
          "Partial term generated : %a"
          Printer.pp_term res ;
        res
    in
    let res =
      __m_to_term (A.P.to_var m)
    in
    Mat_option.debug ~dkey:dkey_term ~level:2
      "Whole term generated : %a"
      Printer.pp_term res ;
    res

  let monomial_to_term (m:A.P.Monom.t) =
    if Mat_option.Linearized_file.get ()
    then
      try
        let var = A.P.Monom.Map.find m (C2A.export_variables ()) in
        Logic_const.term (TLval (TVar (Cil.cvar_to_lvar var),TNoOffset)) Linteger
      with Not_found -> monomial_to_term m
    else monomial_to_term m

  (** Zarith *)
  exception Bad_invariant

  let possible_monomial monom nd_vars =
    let vars = A.P.to_var_set monom in
    List.for_all
      (fun var -> not(var.vtemp) && not (Varinfo.Map.mem var nd_vars))
      vars

  let vec_to_term_zarith
      (rev_base:A.P.Monom.t A.Imap.t)
      (vec : A.M.vec)
      (nd_vars : 'a Varinfo.Map.t)
    =

    let vec_array = A.M.vec_to_array vec in
    let type_is_integer =
      A.Imap.for_all
        (fun row monom ->
           let cst =  vec_array.(row) |> A.P.R.t_to_float
           in
           if cst |> int_of_float |> float_of_int = cst
           then
             if cst = 0. then true
             else
               List.for_all
                 (fun v -> match v.vtype with
                      TInt _ -> true
                    | _ -> false)
                 (A.P.to_var_set monom)

           else false
        )
        rev_base in

    let typ = if type_is_integer then Linteger else Lreal in
    let () = Mat_option.debug ~dkey:dkey_zterm ~level:2
        "Vector given : %a" A.M.pp_vec vec in

    let zero =  Logic_const.term (TConst (Integer (Integer.zero,(Some "0")))) Linteger
    in
    A.Imap.fold
      (fun row monom acc ->
         let cst =

           vec_array.(row) |> A.P.R.t_to_float

         in
         if cst = 0. then acc
         else if not(possible_monomial monom nd_vars) then raise Bad_invariant
         else

           let term_cst =
             let lcst:Cil_types.logic_constant =

               if type_is_integer
               then Integer (Integer.of_int (int_of_float cst), Some (string_of_float cst))
               else LReal
                   {
                     r_literal = string_of_float cst;
                     r_nearest = cst;
                     r_upper = cst;
                     r_lower = cst;
                   }  in

             Logic_const.term
               (TConst lcst) typ in

           let monom_term =

             Logic_const.term
               (TBinOp
                  (Mult,
                   term_cst,
                   monomial_to_term monom)
               )  typ

           in
           if acc = zero then monom_term else
             Logic_const.term (TBinOp (PlusA,acc,monom_term))  typ
      )
      rev_base
      zero

  let get_inst_loc = function
    | Set (_, _, l)
    | Call (_, _, _, l)
    | Skip l
    | Code_annot (_, l) -> l
    (* Between Frama-C Magnesium and Aluminium, the number of fields
       of Asm went from 7 to 4. For the tool to be well typed, the following have
       to be done.
       If you want to treat Asm without failure, then you can use :

       | Asm(_,_,_,_,_,_,l) -> l (Frama-C <= Magnesium)
       | Asm(_,_,_,l) -> l (Frama-C Aluminium)

    *)

    | Asm _ -> assert false
    | Local_init _ -> assert false

  let rec get_stmt_loc s = match s.skind with
    | Instr i -> get_inst_loc i
    | Return (_, l)
    | Goto (_, l)
    | Break l
    | Continue l
    | If (_, _, _, l)
    | Switch(_, _, _, l)
    | Loop (_, _, l, _, _)
    | Throw (_, l)
    | TryCatch (_, _, l)
    | TryFinally (_, _, l)
    | TryExcept (_, _, _, l) -> l
    | Block b ->
      (try
         let first_stmt = List.hd b.bstmts in
         get_stmt_loc first_stmt
       with
       | _ -> raise (Invalid_argument "No statement found"))
    | UnspecifiedSequence s ->
      (try
         let first_stmt, _, _, _, _ = List.hd s in
         get_stmt_loc first_stmt
       with
         _ -> raise (Invalid_argument "No statement found"))


  (** When the invariant is simple (ie sum term = k*t), then the value of
      k can be computed from the initial values of the variables. Returns
      the assignment to add just before the loop starts. *)

  let k_first_value lval term loc =
    let exp = !Db.Properties.Interp.term_to_exp ~result:None term in
    Mat_option.debug
      ~dkey:dkey_term ~level:2
      "Assigning %a to %a = %a"
      Printer.pp_lval lval
      Printer.pp_term term
      Printer.pp_exp exp;

    Instr(Set (lval,exp,loc))

  let add_k_stmt new_ghost_var sum_term stmt =
    let init_k =
      k_first_value
        (Var new_ghost_var,NoOffset)
        sum_term
        (get_stmt_loc stmt)
    in

    init_k

  (** Returns a predicate list based on the term list.
      SUM(ki*ei) is the general invariant, but :
      if the limit is convergent, then each ei is a convergent invariant
      if the limit is divergent, then each ei is a divergent invariant
      else, we use the general invariant *)

  let term_list_to_predicate
      (deter : bool)
      ?(mat : A.M.t option)
      (term_list : (A.M.vec *  term) list)
      (limit : limit)
      (rev_base : A.P.Monom.t A.Imap.t)
      (kf  : Kernel_function.t)
      (stmt : stmt)
      (num_vars:int)
    : (toplevel_predicate * stmtkind option) list =
    let fundec = match kf.fundec with
        Definition(f,_) -> f
      | Declaration _ -> assert false
    in
    match limit,deter with
    | Zero,_ -> (* invar.X = 0 is an invariant *)

      let zero =  (Logic_const.term (TConst (Integer (Integer.zero,(Some "0"))))) Linteger
      in
      List.map
        (fun (_,term) ->
           Logic_const.toplevel_predicate @@
           Logic_const.unamed (Prel(Req,term,zero)),None)
        term_list

    | Altern,_ ->

      let zero =  (Logic_const.term (TConst (Integer (Integer.zero,(Some "0"))))) Linteger
      in
      let term =
        List.fold_left
          (fun acc (_,term) ->
             let new_ghost_var = Cil.makeLocalVar fundec (new_name ()) (TInt (IInt,[]))
             in
             let () =
               new_ghost_var.vghost <- true;
               variables_to_add := new_ghost_var :: !variables_to_add in
             let lvar = Cil.cvar_to_lvar new_ghost_var in
             let term_gvar =
               Logic_const.term
                 (TLval ((TVar lvar),TNoOffset)) Linteger
             in
             let prod_term =
               Logic_const.term
                 (TBinOp
                    (Mult,
                     term_gvar,
                     term)
                 ) Linteger

             in
             if acc = zero then prod_term else
               Logic_const.term
                 (TBinOp (PlusA,acc,prod_term)) Linteger

          )
          zero
          term_list
      in
      let pred =
        Prel
          (Req,
           term,
           zero)
      in

      [Logic_const.toplevel_predicate (Logic_const.unamed pred), None]

    | Convergent ev, false ->

      let make_pred k term =
        let pred =
          Prel
            (Rle,
             term,
             Logic_const.term k Lreal)
        in Logic_const.unamed pred
      in
      begin (* Matching mat *)
        match mat with
          None -> (* This is the intersection of two invariants for non deterministic, existential*)
          List.map
            (fun (_,term) ->
               let k = (Cil.make_temp_logic_var Lreal) in
               let pred = make_pred (TLval (TVar k, TNoOffset)) term in

               Logic_const.toplevel_predicate
                 (Logic_const.unamed
                    (Pexists
                       ([k],pred))),None
            ) term_list

        | Some m ->
          let module NDI = Non_det_invar.Make(A) in
          List.map
            (fun (invar,term) ->

               let k =
                 NDI.do_the_job
                   rev_base
                   m
                   ev
                   invar
                   num_vars
               in
               let k_float = float_of_string k in
               let k_real =
                 {
                   r_literal = k;
                   r_nearest = k_float;
                   r_upper = k_float;
                   r_lower = k_float
                 } in
               Logic_const.toplevel_predicate @@  make_pred (TConst (LReal k_real)) term, None
            ) term_list
      end (* Matching mat *)
    | _,_ ->

      let operator,integer =
        match limit with
        | Convergent _ -> Rle,false
        | Divergent ev -> Rge,(ev = (ev|>int_of_float|>float_of_int))
        | One -> Req,true
        | _ -> assert false
      in
      let type_vars,logic_type_vars =
        if integer then TInt (IInt,[]),Linteger else TFloat (FFloat,[]),Lreal in
      List.map
        (fun (_,term) ->
           let new_ghost_var = Cil.makeLocalVar fundec (new_name ()) type_vars in
           let () =
             new_ghost_var.vghost <- true;
             variables_to_add := new_ghost_var :: !variables_to_add in
           let lvar = Cil.cvar_to_lvar new_ghost_var in
           let term_gvar =
             Logic_const.term
               (TLval ((TVar lvar),TNoOffset))
               logic_type_vars
           in
           let new_skind = add_k_stmt new_ghost_var term stmt
           in

           let pred =
             Prel
               (operator,
                term,
                term_gvar)
           in (Logic_const.toplevel_predicate @@ Logic_const.unamed pred),(Some new_skind)
        ) term_list

  let vec_space_to_predicate_zarith
      (deter : bool)
      (mat : A.M.t option)
      (kf : Kernel_function.t)
      (stmt: Cil_types.stmt)
      (rev_base: A.P.Monom.t A.Imap.t)
      (invar : Invar_utils.invar)
      (nd_vars : 'a Varinfo.Map.t)
      (num_vars : int)
    : (toplevel_predicate * stmtkind option) list =

    let limit,vec_list = invar in

    let term_list =
      List.fold_left
        (fun acc vec -> try (vec,vec_to_term_zarith rev_base vec nd_vars) :: acc with Bad_invariant -> acc)
        []
        vec_list in
    if deter
    then
      term_list_to_predicate
        true
        term_list
        limit
        rev_base
        kf
        stmt
        num_vars
    else
      term_list_to_predicate
        false
        ~mat:(Extlib.the mat)
        term_list
        limit
        rev_base
        kf
        stmt
        num_vars

  let loop_annots_vars_init
      (deter : bool)
      (mat : A.M.t option)
      (kf:kernel_function)
      (stmt:stmt)
      (rev_base:A.P.Monom.t A.Imap.t)
      (vec_lists : Invar_utils.invar list)
      (nd_vars : 'a Varinfo.Map.t)
      (num_vars:  int)
    =


    let vec_lists = (* if we use zarith, we try to simplify the invariants by "integrating" them. *)
      if (Mat_option.Use_zarith.get ())
      then
        List.map
          Invar_utils.integrate_invar
          vec_lists


      else vec_lists in

    let annots,init_list =
      List.fold_left
        (fun acc invar ->
           let annot_list,init_list =
             to_code_annot (
               vec_space_to_predicate_zarith
                 deter
                 mat
                 kf
                 stmt
                 rev_base
                 invar
                 nd_vars
                 num_vars)
           in
           (annot_list@(fst acc),init_list@(snd acc))
        )
        ([],[])
        vec_lists

    in
    (*Stmt.Hashtbl.add annotations_to_emit stmt (kf,annots);*)
    let res = !variables_to_add in variables_to_add := []; annots,res,init_list
end
