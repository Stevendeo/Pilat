include Plugin.Register
  (struct 
    let name = "Pilat"
    let shortname = "pilat"
    let help = "Frama-C Polynomial invariant generator"
   end)

module Enabled = False
  (struct 
    let option_name = "-pilat"
    let help = "when on, generates polynomial invariants for each solvable loop of the program" 
   end)
      
module Degree = Int
  (struct 
    let option_name = "-pilat-degree"
    let help = "sets the degree of the invariants seeked"
    let arg_name = "n"
    let default = 2
   end)

module NameConst = String
  (struct 
    let option_name = "-pilat-const-name"
    let arg_name = "str"
    let default = "__PILAT__"
    let help = "sets the name of the constants used in invariants (default" ^ default ^ ")"
   end)

module Output_C_File =
  Empty_string
    (struct
       let option_name = "-pilat-output"
       let arg_name = ""
       let help = "specifies generated file name for annotated C code"
     end)

(** Tools for ACSL generation *)

let emitter = Emitter.create 
  "Pilat_emitter"
  [Emitter.Code_annot;Emitter.Global_annot]
  ~correctness:
  [Degree.parameter]
  ~tuning:[]

(** Misc. *)

let parsing_timer = ref 0.

let invar_timer = ref 0.

let inter_timer = ref 0.

let nullspace_timer = ref 0.

let ltoq_timer = ref 0.

let ev_timer = ref 0.

let char_poly_timer = ref 0.
