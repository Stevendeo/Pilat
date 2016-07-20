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

open Pilat_matrix 

let get_objective_from_convergent_invar tr_pmat ev invar = 
  let pinvar = fvec_to_pvec invar in 
  let ev_times_invar = 
    fvec_to_pvec 
      (Lacaml_D.Vec.map
	 (fun f -> ev *. f)
	 invar)
  in
  PMat.sub_vec (PMat.mul_vec tr_pmat pinvar) ev_times_invar
  

let get_objectives pmat = 
  let transposed_pmat = PMat.transpose pmat in
  let mat_zero = pmat_eval_to_zero pmat in 
  
  
  let get_invariants = 
    Invariant_utils.invariant_computation_lacaml mat_zero
  in
  
  let objectives = 
    List.fold_left
      (fun acc (lim,invars) -> 
	match lim with
	  Convergent q ->
	    let ev_float = 
	      (Z.to_float (Q.num q)) /. (Z.to_float (Q.den q))
	    in
	    List.fold_left
	      (fun invar