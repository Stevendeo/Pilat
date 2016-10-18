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

open Cil_types 
open Cil_datatype
open Invariant_utils 

module Make: functor (A:Poly_assign.S) -> 
sig
  
  module Invar_utils : S with type mat = A.mat and type invar = A.M.vec inv

  val add_loop_annots :  
    bool ->
    ?mat:A.M.t ->
    Cil_types.kernel_function ->
    Cil_types.stmt ->
    A.P.Monom.t A.Imap.t -> 
    Invar_utils.invar list -> 
    'a Varinfo.Map.t -> 
    unit
    
    
end
