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

include Plugin.Register
  (struct
    let name = "Pilat"
    let shortname = "pilat"
    let help = "Frama-C Polynomial invariant generator"
   end)
(*
module Enabled = False
  (struct
    let option_name = "-pilat"
    let help = "when on, generates polynomial invariants for each solvable loop of the program"
   end)
  *)
module Degree = Int
  (struct 2
    let option_name = "-pilat-degree"
    let help = "sets the maximum degree of the invariant"
    let arg_name = "n"
    let default = -1
   end)

module Output_C_File =
  String
    (struct
       let option_name = "-pilat-output"
       let arg_name = "s"
       let help = "specifies generated file name for annotated C code"
       let default = "whole_program_annot.c"
     end)

module NameConst = String
  (struct
    let option_name = "-pilat-const-name"
    let arg_name = "str"
    let default = "__PILAT__"
    let help = "sets the name of the constants used in invariants (default " ^ default ^ ")"
   end)

module Use_zarith =
  True
    (struct
      let option_name = "-pilat-z"
      let help = "when on, uses zarith library. Recommended if searching for integer relations but depreciated when searching for floating point relations."
   end)

module Ev_leq =
  Int
    (struct
        let option_name = "-pilat-ev"
	let help = "sets the maximal eigenvalue searched for when using zarith"
	let arg_name = "n"
	let default = 10
     end)

module Var_focus =
  Empty_string
    (struct
      let option_name = "-pilat-vars"
      let arg_name = "x:y:..."
      let help = "specifies which variables will be analyzed. If the input is\
                  matricial, uses those names as variables."

     end)

module Prove =
  False
    (struct
      let option_name = "-pilat-prove"
      let help = "when on, tries to prove already existing loop invariants"
     end)

module Redundancy =
  False
    (struct
      let option_name = "-pilat-redun"
      let help = "undocumented"
     end)


module Linearized_file =
  False
    (struct
      let option_name = "-pilat-lin"
      let help = "Outputs the loop-linearized program."
     end)



module Optim_start = String
    (struct
        let option_name = "-pilat-optim-start"
	let default = "50"
	let help = "sets the initial value of k during the optimisation (default "^ default ^")"
	let arg_name = "n"
     end)

module Optim_iters = String
    (struct
        let option_name = "-pilat-optim-iters"
	let default = "10"
	let help = "sets the maximal number of iterations performed during the optimisation (default "^ default ^ ")"
	let arg_name = "n"
     end)

module Optim_epsilon = String
  (struct
    let option_name = "-pilat-optim-epsilon"
    let arg_name = "str"
    let default = "0.05"
    let help = "Tolerance of error during optimization (default " ^ default ^ ")"
   end)

module Mat_input = String
    (struct
      let option_name = "-pilat-input"
      let arg_name = "file"
      let default = ""
      let help = "Specifies the input file if it is one or multiple numerical\
                  matrices. Separate each matrix with ';;' and each line with\
                  ';'."
   end)

(** Tools for ACSL generation *)

let emitter = Emitter.create
  "Pilat_emitter"
  [Emitter.Code_annot;Emitter.Global_annot]
  ~correctness:
  [Degree.parameter]
  ~tuning:[]

(** Misc. *)

let non_det_name = "Frama_C_float_interval"

let var_list () =
  let str = Var_focus.get () in
  let list = Str.split (Str.regexp ":") str
  in
  List.fold_left
    (fun acc str_v ->
      try
	Cil_datatype.Varinfo.Set.add (Globals.Vars.find_from_astinfo str_v Cil_types.VGlobal) acc
      with
	Not_found ->
	  try
	    Cil_datatype.Varinfo.Set.add
	      (Globals.Vars.find_from_astinfo
		 str_v
		 (Cil_types.VLocal (Globals.Functions.find_by_name "main")))
		acc

	  with
	    Not_found ->
	      (feedback "Variable %s not found") str_v; acc)
    Cil_datatype.Varinfo.Set.empty
    list

(** File in which the optimization result is saved *)

let k_file = "k.k"

(** Timers *)

let whole_rel_time = ref 0.

let parsing_timer = ref 0.

let invar_timer = ref 0.

let inter_timer = ref 0.

let nullspace_timer = ref 0.

let ltoq_timer = ref 0.

let ev_timer = ref 0.

let char_poly_timer = ref 0.

let proof_timer = ref 0.

let optimizer_timer = ref 0.

let redun_timer = ref 0.
