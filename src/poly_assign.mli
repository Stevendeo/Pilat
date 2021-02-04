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

exception Missing_variables
exception Incomplete_base
exception Not_solvable

module type S = sig

  (** 1. Utils *)

  module P : Polynomial

  module M : Matrix with type elt = P.c

  module R : Ring with type t = P.c
  (** Takes a monomial and its assignment, returns a matrix and its base.
      If a base is provided it will complete it and use it for the matrix, else it
      will create a new base from the affectation.
      Raises Incomplete_base if unconsidered variables are necessary for the matrix.
  *)
  module Var = P.Var

  val to_mat : ?base:int P.Monom.Map.t -> P.Monom.t -> P.t -> int P.Monom.Map.t * M.t

  type mat = M.t (** Matrix in which the affectation will be translated *)
  type coef = P.c (** Coefficient of the polynomial and of the matrix *)
  type var = P.v (** Variables used by the polynomial *)

  type monomial = P.Monom.t
  type m_set = P.Monom.Set.t
  type p = P.t

  type t =
      Assign of Var.t * P.t
    | Assert of Cil_types.exp * body * body (* Todo : do not depend on Frama-C *)
    | Loop of body
    | Other_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)

  and body = t list

  val pretty_assign : Format.formatter -> t -> unit

  (** A monomial assignment is equivalent to considering a monomial is a variable modified
      by the assignment. *)

  type monom_assign =

      LinAssign of monomial * p
    | LinAssert of Cil_types.exp * lin_body * lin_body (* Todo : do not depend on Frama-C *)
    | LinLoop of lin_body
    | LinOther_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)

  and lin_body = monom_assign list

  val pretty_linassign : Format.formatter -> monom_assign -> unit

  (** For each variable v of the set, returns the assignment v = v. This is needed when
      a variable doesn't appear on each loop body. *)
  val basic_assigns : P.Var.Set.t -> body

  (** Returns the list of monomial assignments needed to linearize the loop, and the
      set of all monomials used. Raises Missing_variables if variables not present in the
      map in argument are used as r-values in the body. *)
  val add_monomial_modifications :
    P.Var.Set.t -> body -> lin_body * P.Monom.Set.t

  module Imap : Map.S with type key = int

  (** Links each monomial to an integer. Used as base for the matrix *)
  val monomial_base : P.Monom.Set.t -> int P.Monom.Map.t

  (** Reverses the base *)
  val reverse_base : int P.Monom.Map.t -> P.Monom.t Imap.t

  (** Given a base, prints a vector as a polynomial *)
  val print_vec : Format.formatter -> P.Monom.t Imap.t * M.vec -> unit

  (** Transforms a vector to a polynomial *)
  val vec_to_poly : P.Monom.t Imap.t -> M.vec -> P.t

  (** Transforms a list of monomial assignments *)
  val loop_matrix : int P.Monom.Map.t -> monom_assign list -> mat list

end

module Make:
  functor
    (M : Matrix)
    (Poly : Polynomial with type c = M.elt) -> S with type P.c = M.elt
                                                  and type P.v = Poly.v
                                                  and type P.Var.Set.t = Poly.Var.Set.t
