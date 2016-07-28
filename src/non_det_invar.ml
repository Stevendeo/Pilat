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
open Invariant_utils 

module Make (P_assign : Poly_assign.S) = 
  struct 
    module M = P_assign.M
    module R = P_assign.P.R
    let get_objective_from_convergent_invar mat ev (invar:M.vec) = 

      let ev_times_invar = 
	M.scal_mul_vec invar (R.float_to_t ev)
      
      in
      let tmat = (M.transpose mat) in
      let tmattinvar = (M.mul_vec tmat invar) in
      M.sub_vec tmattinvar ev_times_invar
	
    let objective_to_string base (objective:M.vec) = 
      let rbase = P_assign.reverse_base base in
      let poly_objective = P_assign.vec_to_poly rbase objective
      in P_assign.P.to_str poly_objective

    let do_the_job base mat ev (invar:M.vec) = 
 
      let obj = get_objective_from_convergent_invar mat ev invar in 

         (objective_to_string base obj)

    let main_constraint_to_string
  end
