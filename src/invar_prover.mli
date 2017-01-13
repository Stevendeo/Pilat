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

module Make : functor (A : Poly_assign.S with type P.v = Cil_datatype.Varinfo.t
					 and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t
) -> 
  sig
    
    val prove_invariant : 
      A.mat -> 
      int A.P.Monom.Map.t -> 
      Cil_types.predicate -> 
      Property_status.emitted_status
	
    val prove_annot : 
      A.mat -> 
      int A.P.Monom.Map.t -> 
      Cil_types.code_annotation -> 
      Property_status.emitted_status

  end
