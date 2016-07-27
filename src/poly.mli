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

open Pilat_math

(** Polynomials with multiple variables. *)    

module type S_with_col_and_nd_rep = 
sig
  include Datatype.S_with_collections
  val non_det_repr : float -> float -> t
end

module Make : functor (A : Ring) (V : S_with_col_and_nd_rep) -> 
  (Polynomial with type c = A.t 
	      and type v = V.t 
	      and type Var.Set.t = V.Set.t) 
    
type var = | X

(** Polynomial with a unique variable, X. *)
module XMake : functor (A : Ring) -> (Polynomial with type c = A.t 
						 and type v = var) 
			
