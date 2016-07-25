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
open Cil_datatype

(** 1. Variables used for polynomials *)

type n_var = 	
  {
    name:string;
    min:float;
    max:float
  }

module N_var : 
  (sig
    include Datatype.S_with_collections
    val new_var : float -> float -> t
   end)

(** 2. Polynomials *) 

(** Polynomials for deterministic assignments *)
module F_poly : Polynomial with type c = float and type v = Varinfo.t

(** Polynomials for non deterministic expressions *)
module N_poly : Polynomial with type c = float and type v = N_var.t

(** Polynomials for non deterministic assignments *)
module NF_poly : Polynomial with type c = N_poly.t and type v = Varinfo.t
