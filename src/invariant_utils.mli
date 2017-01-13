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

type limit = 
  Convergent of float
| Divergent of float
| Altern
| One
| Zero

type 'v inv = limit * 'v list

type q_invar = Pilat_matrix.QMat.vec inv

module type S = 
sig 
  
(** An invariant is an eigenspace, represented by its base with
    a vec list. 
    When an eigenspace is associated to an eigenvalue strictly 
    lower to one, the invariant is convergent 
    (<e,X> < k => <e,MX> <k).
    When it is higher to one, it is divergent
    (<e,X> > k => <e,MX> > k).
*)
  type mat
  type invar
    
  val lim_to_string : limit -> string
    
(** Returns the rational eigenspaces union of the floating matrix 
    as a list of bases. If assignments are non deterministic, the boolean 
    must be set to true *)
  val invariant_computation : bool -> mat -> invar list
    
(** Intersects two union of vectorial spaces. *)
  val intersection_invariants :  invar list -> invar list -> invar list

  val zarith_invariant : invar -> q_invar
  val to_invar : q_invar -> invar 

  val integrate_invar : invar -> invar
end

module Make : functor 
    (A : Poly_assign.S) -> S with type invar = A.M.vec inv and type mat = A.mat
