include Plugin.S

module Enabled: Parameter_sig.Bool
 
module Degree: Parameter_sig.Int

module NameConst: Parameter_sig.String

module Output_C_File: Parameter_sig.String

(** Tools for ACSL generation *)

val emitter : Emitter.t

(** Misc. *)

val parsing_timer : float ref

val invar_timer : float ref 

val inter_timer : float ref 

val nullspace_timer : float ref

val ltoq_timer : float ref

val ev_timer : float ref

val char_poly_timer : float ref
