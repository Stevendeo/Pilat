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

open Cil_datatype

  val emit_annot_list : 
    Cil_types.code_annotation list -> Cil_types.stmt -> Kernel_function.t -> unit
  
module Make: functor 
  (A:Poly_assign.S with type P.v = Varinfo.t)
  (C2A: sig val export_variables : unit -> Cil_types.varinfo A.P.Monom.Map.t end) -> 
sig
  
  val loop_annots_vars_init :  
    bool ->
    A.M.t option ->
    Cil_types.kernel_function ->
    Cil_types.stmt ->
    A.P.Monom.t A.Imap.t -> 
    A.M.vec Invariant_utils.invariant list -> 
    'a Varinfo.Map.t -> 
    int -> 
    Cil_types.code_annotation list * Varinfo.t list * Cil_types.stmtkind list
  
     
end
