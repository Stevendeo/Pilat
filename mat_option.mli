include Plugin.S

module Enabled: Parameter_sig.Bool
 
module Degree: Parameter_sig.Int

module NameConst: Parameter_sig.String

module Output_C_File: Parameter_sig.String

module Use_zarith: Parameter_sig.Bool

(** Tools for ACSL generation *)

val emitter : Emitter.t
