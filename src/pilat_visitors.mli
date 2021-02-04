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

open Cil_types
open Cil_datatype

val studied_variables : block -> Varinfo.Set.t * (float*float) Varinfo.Map.t

(** Used to register generated assignments to loop heads. *)
val register_stmt : stmt -> stmtkind -> unit

val make_assign_block : stmtkind list -> stmt -> stmt

(** fundec_updater adds the constant initialisation as new stmts in the CFG. It is based on the
    stmts registered by register_stmt
class fundec_updater : Project.t -> Visitor.frama_c_copy
*)
