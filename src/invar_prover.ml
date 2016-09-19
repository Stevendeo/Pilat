(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Poly_affect

exception Bad_invariant

let poly_of_cst = function
  | Integer (i,_) -> i |> Integer.to_float |> F_poly.const
  | LStr _ | LWStr _ | LChr _ -> raise Bad_invariant
    
  (* We can do a better job by searching for the best real for which it is
     an invariant. For now, we will just try on the nearest float. *)
  | LReal lr -> F_poly.const lr.r_nearest
  | LEnum enumitem -> Matrix_ast.exp_to_poly enumitem.eival

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
  | TVar {lv_origin = Some v} -> F_poly.monomial 1. [v,1]

let cast_poly typ p = 
  match typ with
  | TInt _ -> (* Cast floats to int *)
    F_poly.Monom.Set.fold
      (fun m acc -> 
	let coef = F_poly.coef p m in 
	let casted = 
	  if coef < 0. then ceil coef
	  else floor coef 
	in
	F_poly.add acc (F_poly.mono_poly casted m)
      )
      (F_poly.get_monomials p)
      F_poly.zero
  | TFloat _ -> p
  | _ -> raise Bad_invariant
	
    
let rec poly_of_term t = match t.term_node with
  | TConst lc -> poly_of_cst lc
  | TLval tl -> poly_of_termlval tl
  | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ -> 
    raise Bad_invariant
  | TUnOp (Neg,t) -> F_poly.sub (F_poly.zero) (poly_of_term t)
  | TUnOp _ -> raise Bad_invariant
  | TBinOp (PlusA,t1,t2) -> 
    F_poly.add (poly_of_term t1) (poly_of_term t2)
  | TBinOp (MinusA,t1,t2) -> 
    F_poly.sub (poly_of_term t1) (poly_of_term t2)
  | TBinOp (Mult,t1,t2) -> 
    F_poly.mul (poly_of_term t1) (poly_of_term t2)
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
  | TCoerce (t,_) | TLogic_coerce (_,t) -> poly_of_term t
  | _ -> 
    let () = Mat_option.feedback "%a term not supported" 
      Printer.pp_term t 
    in raise Bad_invariant
  

let poly_of_pred (pred:predicate) = 
  match pred with
    Prel (_,tl,tr) -> 
      F_poly.sub (poly_of_term tl) (poly_of_term tr)
  | _ -> assert false
  
let predicate_to_vector (base:int F_poly.Monom.Map.t) (pred:predicate) =  
  let poly = poly_of_pred pred in 
  let p_monoms  = F_poly.get_monomials poly in 
  let res = Lacaml_D.Vec.init (F_poly.Monom.Map.cardinal base) (fun _ -> 0.) in
  F_poly.Monom.Set.iter
    (fun m -> 
      try 
	let pos = F_poly.Monom.Map.find m base in
	res.{pos}<-F_poly.coef poly m
      with Not_found -> 
	let () = Mat_option.feedback 
	  "Monomial %a not in the base in argument. Considered constant"
	  F_poly.pp_print (F_poly.mono_poly 1. m)
	in
	(*raise Bad_invariant*) ()
    ) p_monoms; res

(* This exception is raised to stop the iteration of the next function when we can conclude
   this is not an invariant. If it is not raised, then it is an invariant. *)
exception Not_an_invariant
let prove_invariant (mat:Lacaml_D.Mat.t) (base:int F_poly.Monom.Map.t) (pred:predicate) = 

  let t0 = Sys.time () in
  try 
  let vec = predicate_to_vector base pred in

  let matt = Lacaml_D.Mat.transpose_copy mat in
  
  let mtvec = Lacaml_D.gemv matt vec in 
   
  let index = ref 0 in 
  let res = 
    try
      ignore( 
	Lacaml_D.Vec.fold
	  (fun acc c_mtvec -> (* acc will store the possible eigenvalue *)
	    index := !index + 1;
	    let c_vec = vec.{!index} in
	    
	    if c_mtvec = 0. then
	      begin
		if c_vec = 0. then acc (* 0*ev = 0, we don't know the ev *)
		else Some 0. (* c_vec * ev = 0 => ev = 0. *)
	      end
	    else 
	      let ev = c_vec /. c_mtvec in 
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
  in
  Mat_option.proof_timer := !Mat_option.proof_timer +. Sys.time () -. t0; res
    
  with 
  | Bad_invariant -> 
    Mat_option.proof_timer := !Mat_option.proof_timer +. Sys.time () -. t0; 
    Property_status.Dont_know 
      
let prove_annot mat map annot = 
  match annot.annot_content with
    AInvariant (_,_,p) -> 
      prove_invariant mat map p.content
  | _ -> raise Bad_invariant
