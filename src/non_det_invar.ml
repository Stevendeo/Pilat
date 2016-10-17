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

open Poly_utils 

let dkey = Mat_option.register_category "ndi:do_the_job"

module Make (P_assign : Poly_assign.S) = 
  struct 
    module M = P_assign.M
    module P = P_assign.P
    module R = P.R
    let get_objective_from_convergent_invar mat ev (invar:M.vec) = 

      let ev_times_invar = 
	M.scal_mul_vec invar (R.float_to_t ev)
      in
      let tmat = (M.transpose mat) in
      let tmattinvar = (M.mul_vec tmat invar) in
      M.sub_vec tmattinvar ev_times_invar

    let vec_to_poly_string rev_base vec = 
      let poly = P_assign.vec_to_poly rev_base vec
      in let () = Mat_option.feedback "Poly : %a" P.pp_print poly
      in P_assign.P.to_str poly

    let lower_bound (n:N_poly.Var.t) = 
      let var_str = N_poly.monomial N_poly.R.one [(n,1)] |> N_poly.to_str
      in
      var_str ^ "-(" ^ string_of_float (N_poly.Var.min n) ^ ")"

    let upper_bound (n:N_poly.Var.t) = 
      let var_str = N_poly.monomial N_poly.R.one [(n,1)] |> N_poly.to_str
      in string_of_float (N_poly.Var.min n) ^ "-" ^ var_str

    let set_of_n_in_vec (vec:M.vec) = 
      let arr = M.vec_to_array vec in 
      Array.fold_right
	(fun p acc -> 
	  R.to_nvars p |> N_poly.Var.Set.of_list |> N_poly.Var.Set.union acc
	)
	arr
	N_poly.Var.Set.empty
      
    let do_the_job rev_base (mat:M.t) (ev:float) (invar:M.vec) = 
      let time = Sys.time () in
      let (minus_one:R.t) = R.sub R.zero R.one in 
      let objective = 
	M.scal_mul_vec (get_objective_from_convergent_invar mat ev (invar:M.vec)) minus_one
      in
      let obj_str = vec_to_poly_string rev_base objective
      in
      let invar_constraint = 
	M.scal_mul_vec invar minus_one
      in
      let invar_str = vec_to_poly_string rev_base invar_constraint
      in
      let nd_vars = set_of_n_in_vec objective in 
      let nd_cons = 
	N_poly.Var.Set.fold
	  (fun n acc -> acc ^ "\" " ^ lower_bound n ^ "\" " ^ "\"" ^ upper_bound n ^ "\" ")
	  nd_vars 
	  "" in
      
      let prefix = "sage optimizer.py 0 " ^ (string_of_float ev) 
      in
      let max_k = "50" in
      let n = "10" in 
      let suffix = max_k ^ " "
      ^ n ^ " " 
      ^ "\"" ^ obj_str ^ "\" " 
      ^ "\"" ^ invar_str ^ "\" "  ^ nd_cons
      in
      let regexp = Str.regexp "\\[.\\]" in
      let rec max_tab_index str_index tab_index = 
	try 
	  let new_index = Str.search_forward regexp suffix str_index in
	  let index_str = Str.matched_string suffix in
	  let index_int = 
	    String.sub 
	      index_str 
	      1 
	      (String.length index_str - 2) |> int_of_string in 
	  max_tab_index (new_index + 3) (max tab_index index_int)
	  
	with
	  Not_found -> tab_index
      in
      let res = 
	prefix ^ " " ^ string_of_int ((max_tab_index 0 0) + 1) ^ " " ^ suffix
      in
      let () = 
	    Mat_option.feedback "Searching for a value of k. May take some time..." in
	 
	  let () = 
	    Mat_option.debug ~level:3 ~dkey "Command line = %s" res in

	  let () = ignore (Sys.command (res ^ " > " ^ Mat_option.k_file)) in
	  let in_channel = open_in "k.k" in
	  
	  let k = ref "" in 
	  let () = 
	    try 
	      while true do 
		k := input_line in_channel
	      done
	    with End_of_file -> close_in in_channel
	  in
	  let () = 
	    Mat_option.feedback "k = %s" !k; 
	    let time = 
	      Sys.time () -. time in 
	    Mat_option.debug ~dkey ~level:2
	      "Time for optimizing : %f" time;
	    Mat_option.optimizer_timer := time +. !Mat_option.optimizer_timer;
	  in
	  !k

  end
