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

module Float_deterministic : Poly_assign.S with type P.v = Cil_datatype.Varinfo.t
			  and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t

module Q_deterministic : Poly_assign.S with type P.v = Cil_datatype.Varinfo.t
			  and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t

module Float_non_deterministic : Poly_assign.S with type P.c = Poly_utils.N_poly.t
					       and type P.v = Cil_datatype.Varinfo.t
			  and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t

module Q_non_deterministic : Poly_assign.S  with type P.v = Cil_datatype.Varinfo.t
			  and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t

module Determinizer : functor (ND_assign :Poly_assign.S) ->
  sig
    val nd_mat_to_d_mat : ND_assign.M.t -> Float_deterministic.M.t
  end
