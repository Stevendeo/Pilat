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

open Cil_datatype
open Pilat_math

exception Incomplete_base


module type S = sig 

  type coef (** Coefficient of the polynomial *)
  type var (** Variables used by the polynomial *)

  type mat (** Matrix in which the affectation will be translated *)

  module P : 
    (sig 
      include Polynomial       
  (** Takes a monomial and its affectation, returns a matrix and its base. 
      If a base is provided it will complete it and use it for the matrix, else it 
      will create a new base from the affectation.
      Raises Incomplete_base if unconsidered variables are necessary for the matrix.
  *)
      val to_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * mat
     end)

  type t = Affect of var * P.t
  and body = t list

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
  type monom_affect = P.Monom.t * P.t

end
  
module Make: 
  functor (P : Polynomial)(M : Matrix with type elt = P.c) -> 
    S with type coef = P.c 
      and type var = P.v
      and type mat = M.t
  
