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

type 'a lim = 
  Convergent of 'a
| Divergent of 'a
| Altern
| One
| Zero

type ('a,'v) inv = 'a lim * 'v list

type q_invar = (Q.t,QMat.vec) inv

module Make : functor 
    (A : Poly_assign.S) -> 
sig 
  
(** An invariant is an eigenspace, represented by its base with
    a vec list. 
    When an eigenspace is associated to an eigenvalue strictly 
    lower to one, the invariant is convergent 
    (<e,X> < k => <e,MX> <k).
    When it is higher to one, it is divergent
    (<e,X> > k => <e,MX> > k).
*)

  type limit = A.P.R.t lim
      
  type invar = (A.P.R.t,A.M.vec) inv
    
  val lim_to_string : limit -> string
    
(** Returns the rational eigenspaces union of the floating matrix 
    as a list of bases. *)
  val invariant_computation : A.mat -> invar list
    
(** Intersects two union of vectorial spaces. *)
  val intersection_invariants :  invar list -> invar list -> invar list

  val zarith_invariant : invar -> q_invar
end

    (*
  (** After the integration, there is no fraction left on the vector expression. *)
  val integrate_vec : q_vec -> q_vec
    *)
val integrate_vec : QMat.vec -> QMat.vec
