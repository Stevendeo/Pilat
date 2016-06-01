open Cil_types
open Cil_datatype

val varinfo_registerer : block -> Varinfo.Set.t

(** fundec_updater adds the constant initialisation as new stmts in the CFG. It is based on the 
    stmts registered by register_stmt *)

val register_stmt : stmt -> stmtkind -> unit
val register_annot : stmt -> code_annotation list -> unit
class fundec_updater : Project.t -> Visitor.frama_c_copy
