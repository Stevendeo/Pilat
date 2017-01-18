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

include Plugin.S

(** 1. Parameters *)

module Enabled: Parameter_sig.Bool
 
module Degree: Parameter_sig.Int

module NameConst: Parameter_sig.String

module Output_C_File: Parameter_sig.String

module Use_zarith: Parameter_sig.Bool

module Ev_leq: Parameter_sig.Int

module Var_focus : Parameter_sig.String

module Prove : Parameter_sig.Bool

module Redundancy : Parameter_sig.Bool

module Optim_start: Parameter_sig.String
module Optim_iters: Parameter_sig.String
module Optim_epsilon: Parameter_sig.String

(** Tools for ACSL generation *)

val emitter : Emitter.t

(** Misc. *)

(** Name of the non deterministic function *)

val non_det_name : string

(** List of variables studied *)

val var_list : unit -> Cil_datatype.Varinfo.Set.t

(** File in which the optimization result is saved *)

val k_file : string

(** Timers *)

val whole_rel_time : float ref

val parsing_timer : float ref

val invar_timer : float ref 

val inter_timer : float ref 

val nullspace_timer : float ref

val ltoq_timer : float ref

val ev_timer : float ref

val char_poly_timer : float ref

val proof_timer : float ref

val optimizer_timer : float ref

val redun_timer : float ref
