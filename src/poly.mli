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

open Pilat_math

(** Polynomials with multiple variables. *)    
    
module Make : functor (A : Ring) (V : Pilat_math.Variable) -> 
  (Polynomial with type c = A.t 
	      and type v = V.t 
	      and type Var.Set.t = V.Set.t) 

(** Polynomial with a unique variable, X. *)
module XMake : functor (A : Ring) -> (
    sig 
      include Polynomial with type c = A.t and type v = unit

      (** Simple version of the polynomial euclidian division. div a b = (q,r) where a = qb + r *)
      val div : t -> t -> (t*t) 

      (** Simple version of the polynomial evaluation *)
      val eval : t -> v -> v 

      (** Returns the multiplicity of a given root. Returns 0 if it is not a root. *)
      val root_multiplicity : t -> v -> int

      (** Returns false if there exist roots strictly higher than one. 
          For generalized eigenvectors, knowing if all roots of a polynomial 
          are negative allows to give new invariants.  *)
      val negative_roots : t -> bool
    end 
  ) 
			
