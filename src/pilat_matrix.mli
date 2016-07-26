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

(** 1. Matrix functor *)

(** Given a ring, creates a matrix module.
    Iterations are made line by line. *)
module Make :
  functor (F : Ring) -> Matrix with type elt = F.t
  

(** 2. Rational matrix implementation *)

(** Matrix with rational coefficients
    The module Q is from the Zarith library *)
module QMat : Matrix with type elt = Q.t

(** Set of rational numbers *)
module Q_Set:Set.S with type elt = Q.t

(** Matrix utilities *)

(** Characteristic polynomial of a rational matrix *)
val char_poly : QMat.t -> Poly_utils.XQ_poly.t

(** Rational eigenvalues of a matrix. Computation of the 
    rational roots of the characteristic polynomial.
      ^
     /|\
    /_o_\ 
    
    If the characteristic polynomial is to big, will only test
    a subset of all possible eigenvalues.
*)
val eigenvalues : QMat.t -> Q_Set.t

(** Float <-> Rational translaters *)
val lvec_to_qvec : Lacaml_D.vec -> QMat.vec
val qvec_to_lvec : QMat.vec -> Lacaml_D.vec 

val lacaml_to_qmat : Lacaml_D.Mat.t -> QMat.t
val qmat_to_lacaml : QMat.t -> Lacaml_D.Mat.t

(** 3. Polynomial matrices implementation. 
    This is how we will deal with non deterministic loops *)

module PMat : Matrix with type elt = Poly_utils.N_poly.t

module PQMat :  Matrix with type elt = Poly_utils.NQ_poly.t

val pmat_eval_to_zero : PMat.t -> Lacaml_D.Mat.t
val fvec_to_pvec : Lacaml_D.Vec.t -> PMat.vec 
