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
open Cil_datatype

(** 1. Variables used for polynomials *)

module N_var : Variable with type t = n_var

(** 2. Polynomials *)

(** Polynomials for deterministic assignments *)

(** Polynomial with rational coefficients *)
module QPoly : Polynomial with type c = Q.t
			  and type v = Cil_datatype.Varinfo.t
			  and type Var.Set.t = Varinfo.Set.t

module F_poly : Polynomial with type c = float
			   and type v = Varinfo.t
			   and type Var.Set.t = Varinfo.Set.t

(** Polynomials for non deterministic expressions *)
module N_poly : Polynomial with type c = float and type v = N_var.t

module NQ_poly : Polynomial with type c = Q.t and type v = N_var.t

(** Polynomials for non deterministic assignments *)
module NF_poly : Polynomial with type c = N_poly.t
			    and type v = Varinfo.t
			    and type Var.Set.t = Varinfo.Set.t


module NQF_poly : Polynomial with type c = NQ_poly.t
			     and type v = Varinfo.t
			     and type Var.Set.t = Varinfo.Set.t
