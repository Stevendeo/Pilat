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
open Cil_datatype
exception Bad_invariant

module Make (A : Poly_assign.S with type P.v = Cil_datatype.Varinfo.t
                                and type P.Var.Set.t = Varinfo.Set.t) =
struct
  module Cil2Ply = Cil2assign.Make(A)

  let poly_of_cst =
    function
    | Integer (i,_) ->
      let fl = Integer.to_float i in
      let cst = (A.P.R.float_to_t fl) in A.P.const cst
    | LStr _ | LWStr _ | LChr _ -> raise Bad_invariant

    (* We can do a better job by searching for the best real for which it is
       an invariant. For now, we will just try on the nearest float. *)
    | LReal lr -> A.P.const (A.P.R.float_to_t lr.r_nearest)
    | LEnum enumitem ->
      (* This case shouldn't contain any variable, so empty map *)
      Cil2Ply.exp_to_poly A.Var.Map.empty enumitem.eival

  let poly_of_termlval (term_lhost,t_offset) =
    let () =
      match t_offset with
        TNoOffset -> ()
      | TField _ | TModel _ | TIndex _ ->
        let () =
          Mat_option.feedback
            "Logic term %a cannot be treated."
            Printer.pp_term_lval (term_lhost,t_offset)

        in
        raise Bad_invariant
    in
    match term_lhost with
      TResult _ | TMem _ -> raise Bad_invariant
    | TVar {lv_name = s; lv_origin = None} ->
      let () =
        Mat_option.feedback
          "%s is not a real C variable." s
      in
      raise Bad_invariant
    | TVar {lv_origin = Some v} -> A.P.monomial A.P.R.one [v,1]

  let cast_poly typ p =
    match typ with
    | TInt _ -> (* Cast floats to int *)
      (*A.P.Monom.Set.fold
        (fun m acc ->
          let coef = A.P.coef p m in
          let casted = A.P.R.approx coef
          in
          A.P.add acc (A.P.mono_poly casted m)
        )
        (A.P.get_monomials p)
        A.P.zero*) raise Bad_invariant
    | TFloat _ -> p
    | _ -> raise Bad_invariant


  let rec poly_of_term t = match t.term_node with
    | TConst lc -> poly_of_cst lc
    | TLval tl -> poly_of_termlval tl
    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
      raise Bad_invariant
    | TUnOp (Neg,t) -> A.P.sub (A.P.zero) (poly_of_term t)
    | TUnOp _ -> raise Bad_invariant
    | TBinOp (PlusA,t1,t2) ->
      A.P.add (poly_of_term t1) (poly_of_term t2)
    | TBinOp (MinusA,t1,t2) ->
      A.P.sub (poly_of_term t1) (poly_of_term t2)
    | TBinOp (Mult,t1,t2) ->
      A.P.mul (poly_of_term t1) (poly_of_term t2)
    | TBinOp _ -> raise Bad_invariant
    | TCastE (typ,t) -> t |> poly_of_term |> (cast_poly typ)
    | TAddrOf _ ->
      let () = Mat_option.feedback "%a : adresses not supported"
          Printer.pp_term t
      in raise Bad_invariant
    | TStartOf _ ->
      let () = Mat_option.feedback "%a : arrays not supported"
          Printer.pp_term t
      in raise Bad_invariant
    | Tapp _ ->
      let () = Mat_option.feedback "%a : logic functions not supported"
          Printer.pp_term t
      in raise Bad_invariant
    | Tlambda _ ->
      let () = Mat_option.feedback "%a : lambda abstractions not supported"
          Printer.pp_term t
      in raise Bad_invariant
    | TLogic_coerce (_,t) -> poly_of_term t
    | _ ->
      let () = Mat_option.feedback "%a term not supported"
          Printer.pp_term t
      in raise Bad_invariant


  let poly_of_pred (pred:predicate) =
    match pred.pred_content with
      Prel (_,tl,tr) ->
      A.P.sub (poly_of_term tl) (poly_of_term tr)
    | _ -> assert false

  let predicate_to_vector (base:int A.P.Monom.Map.t) (pred:predicate) =
    let poly = poly_of_pred pred in
    let p_monoms  = A.P.get_monomials poly in
    let res = A.M.create_vec (A.P.Monom.Map.cardinal base) (fun _ -> A.P.R.zero) in
    A.P.Monom.Set.iter
      (fun m ->
         try
           let pos = A.P.Monom.Map.find m base in
           A.M.set_coef_vec pos res (A.P.coef poly m)
         with Not_found ->
           let () = Mat_option.feedback
               "Monomial %a not in the base in argument."
               A.P.pp_print (A.P.mono_poly A.P.R.one m)
           in
           raise Bad_invariant
      ) p_monoms; res

  (* This exception is raised to stop the iteration of the next function when we can conclude
     this is not an invariant. If it is not raised, then it is an invariant. *)
  exception Not_an_invariant
  let prove_invariant (mat:A.M.t) (base:int A.P.Monom.Map.t) (pred:predicate) =
    try
      let vec = predicate_to_vector base pred in

      let matt = A.M.transpose mat in

      let mtvec = A.M.mul_vec matt vec in

      let index = ref 0 in
      try
        ignore(
          A.M.fold_vec
            (fun acc c_mtvec -> (* acc will store the possible eigenvalue *)
               index := !index + 1;
               let c_vec = A.M.get_coef_vec !index vec in

               if c_mtvec = A.P.R.zero then
                 begin
                   if c_vec = A.P.R.zero then acc (* 0*ev = 0, we don't know the ev *)
                   else Some A.P.R.zero (* c_vec * ev = 0 => ev = 0. *)
                 end
               else
                 let ev = A.P.R.div c_vec c_mtvec in
                 (* Floating point division : maybe use zarith instead ? *)
                 match acc with
                   None -> Some ev (* We had no information about the eigenvalue before *)
                 | (Some e) ->
                   if e = ev
                   then acc (* Vectors seem to be colinear, with coefficient ev *)
                   else raise Not_an_invariant (* Vectors are not colinear *)
            )
            None
            mtvec
        );Property_status.True
      with

      | Not_an_invariant -> Property_status.False_if_reachable

    with
    | Bad_invariant ->
      Property_status.Dont_know

  let prove_annot mat map annot =
    match annot.annot_content with
      AInvariant (_,_,p) ->
      let res = prove_invariant mat map p.tp_statement in
      let () =
        Mat_option.feedback "Status of invariant %a : %s"
          Printer.pp_predicate p.tp_statement
          (match res with Property_status.True -> "True" | Property_status.False_if_reachable -> "False" | Property_status.Dont_know -> "Don't know" | _ -> assert false)
      in res


    | _ -> raise Bad_invariant

end
