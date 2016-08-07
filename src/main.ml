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
open Cil
open Pilat_matrix
open Poly_affect

(*open Logic_const
*)
let dkey_stmt = Mat_option.register_category "main:loop_analyser"
let dkey_time = Mat_option.register_category "main:timer"
let dkey_base = Mat_option.register_category "main:base"

let output_fun chan = Printf.fprintf chan "%s\n" 
  
let read_file chan =
  let lines = ref [] in
  try
    while true; do
      lines := input_line chan :: !lines
    done; 
    List.fold_right
      (fun str acc -> acc ^ str ^ "\n") 
      !lines 
      ""
  with End_of_file ->
    List.fold_right
      (fun str acc -> acc ^ str ^ "\n") 
      !lines 
      ""

module Imap = Map.Make(struct type t = int let compare = compare end)

let rev_base base = 

  Poly_affect.F_poly.Monom.Map.fold
    (fun monom i intmap -> 
      Mat_option.debug ~level:5 "Basis %i : %a" 
	i 
	Poly_affect.F_poly.Monom.pretty monom; 
      Imap.add i monom intmap
    )
    base
    Imap.empty 

let print_vec_lacaml rev_base vec = 

  let i = ref 0 in
  Array.iter
    (fun fl ->
      i := !i + 1;if Q.equal fl Q.zero
      then () 
      else 
	Mat_option.debug ~dkey:dkey_stmt
	  "+%f%a" 
	   ((Z.to_float (Q.num fl)) /. (Z.to_float (Q.den fl)))
	 Poly_affect.F_poly.Monom.pretty 
	  (Imap.find !i rev_base)
    ) (QMat.vec_to_array vec)
    
let print_vec_zarith rev_base vec = 

  let i = ref 0 in
  
  Array.iter
    (fun fl ->
      i := !i + 1;
      if Q.equal fl Q.zero
      then () 
      else 
	Mat_option.debug ~dkey:dkey_stmt
	  "+%a%a" 
	  Q.pp_print fl 
	  Poly_affect.F_poly.Monom.pretty 
	  (Imap.find !i rev_base)
    ) (QMat.vec_to_array vec)
    
let print_vec = 
  if Mat_option.Use_zarith.get () 
  then print_vec_zarith
  else print_vec_lacaml

(** Visitor *)
    
let loop_analyzer () = 
object(self)
  inherit Visitor.frama_c_inplace
    
  (*method! vfunc fundec = 
    Cfg.prepareCFG fundec; DoChildren
  *)
  method! vstmt_aux stmt =
    let kf = Extlib.the self#current_kf in
    match stmt.skind with
    | Cil_types.Loop (_,b,_,_,_) -> 
      
      
      let t0 = Sys.time() in

      
      begin (* Loop treatment *)
	let () = 	
	  Mat_option.debug ~dkey:dkey_stmt "Loop %a studied"
	    Cil_datatype.Stmt.pretty stmt;
	  List.iter
	    (fun s -> 
	      Mat_option.debug ~dkey:dkey_stmt ~level:5 "Stmt in loop = %a"
		Cil_datatype.Stmt.pretty s;)
	    b.bstmts
	in
	
	let varinfos_used = Pilat_visitors.varinfo_registerer b in
	Mat_option.debug ~dkey:dkey_stmt ~level:2 "Used varinfos computed";
	
	Cil_datatype.Varinfo.Set.iter
	  (fun v -> 
	    Mat_option.debug 
	      ~dkey:dkey_stmt 
	      ~level:3 
	      "Var %a" 
	      Printer.pp_varinfo v) varinfos_used ;

	(** 1st step : Computation of the block as a list of list of polynomials affectations. *)
	let polys_opt = 
	try Some (Matrix_ast.block_to_poly_lists varinfos_used b)
	with Matrix_ast.Not_solvable -> None 
	in
	
	match polys_opt with 
	  None -> 
	    Mat_option.debug ~dkey:dkey_stmt "The loop is not solvable"; DoChildren

	| Some body_list -> 
	  Mat_option.debug ~dkey:dkey_stmt "The loop is solvable";
	  
	  let varinfos_used = Pilat_visitors.varinfo_registerer b in
	  Mat_option.debug ~dkey:dkey_stmt ~level:2 "Used varinfos computed";
	  
	  Cil_datatype.Varinfo.Set.iter
	    (fun v -> 
	      Mat_option.debug 
		~dkey:dkey_stmt 
		~level:3 
		"%a" 
		Printer.pp_varinfo v) varinfos_used ;

	  let basic_assigns = 
	    (* In order to compute the transformations for all variables in each 
	       loop, even if a variable doesn't appear on all loops, we need to 
	       add identity assignment *)
	    Cil_datatype.Varinfo.Set.fold
	      (fun v acc ->
		let v_monom = (Poly_affect.F_poly.monomial 1. [v,1]) in
		Affect (v,v_monom):: acc )
	      varinfos_used
	      []
	      	    
	  in
	  let body_list = 
	    List.map
	      (fun body -> basic_assigns @ body)
	      body_list
	  in
	  let lin_affects,monom_used = 
	    Matrix_ast.add_monomial_modifications body_list in
	  
          let base = 
	    let i = ref 0 in
	    Poly_affect.F_poly.Monom.Set.fold
	      (fun m map -> 
		i := !i + 1;
		Mat_option.debug ~dkey:dkey_base 
		  "%i <-> %a" !i Poly_affect.F_poly.Monom.pretty m;
	        Poly_affect.F_poly.Monom.Map.add m !i map
	      )
	      monom_used
	      Poly_affect.F_poly.Monom.Map.empty
	  in
	  let rev_base = rev_base base in

	  let matrices = 
	    List.flatten 
	      (List.map
	      (Matrix_ast.loop_matrix base)
	      lin_affects) in
	  if Mat_option.Prove.get () 
	  then
	    let () = Mat_option.feedback "Proving invariants" in
	    let open Property_status in
	    List.iter
	      (fun annot ->
		let status = 
		  List.fold_left 
		    (fun acc mat -> 
		      match acc with
			False_and_reachable | False_if_reachable -> acc
		    | Dont_know | True -> 
		      begin
			match Invar_prover.prove_annot mat base annot with
			  True -> acc
			| res -> res
		      end
		    )
		    True
		    matrices
		in
		
		let () = 
		  Mat_option.feedback
		    "Invariant %a status : %s"
		    Printer.pp_code_annotation annot
		    (match status with
		    True -> "True"
		    | Dont_know -> "?"
		    | _ -> "False") in
		
		let emitter = Annotations.emitter_of_code_annot annot stmt 
		and ip = Property.ip_of_code_annot_single kf stmt annot
		in
		Property_status.emit emitter ~hyps:[] ip status
	      )
	      (Annotations.code_annot stmt); DoChildren
	  else
	    let () = Mat_option.feedback "Invariant generation" in
	    
	    let whole_loop_invar = 
	    List.fold_left
	      (fun acc mat -> 
		if acc = Some [] then Some [] 
		else	  
		  let () = 
		    Mat_option.debug ~dkey:dkey_stmt ~level:3 "New mat : %a" Lacaml_D.pp_mat mat
		  in
 		  let invar = (Invariant_utils.invariant_computation mat)
		  in 

		  Mat_option.debug ~dkey:dkey_stmt ~level:2 "Invar : ";
		  List.iteri
		    (fun i (limit,invars) -> 
		      let () = 
			Mat_option.debug ~dkey:dkey_stmt
			  "Invariant %s %i :" (Invariant_utils.lim_to_string limit)  (i + 1) in
		      List.iter
			(fun invar ->  
			  print_vec rev_base invar;
			  Mat_option.debug ~dkey:dkey_stmt "__\n";
			)invars
			
		    ) invar;
		   match acc with
		     None -> Some invar
		   | Some l ->  
		     Some (Invariant_utils.intersection_invariants invar l) 		  
		  
	      )
	      None
	      matrices
	  in
	  
	  Mat_option.whole_rel_time := Sys.time() -. t0 +. ! Mat_option.whole_rel_time ;
	  match whole_loop_invar with 
	    None -> DoChildren 
	  | Some i ->  Acsl_gen.add_loop_annots_zarith kf stmt base i; DoChildren
	  
      end (* Loop *)
    | _ -> DoChildren 
end
     
let run () =  
  if Mat_option.Enabled.get ()
  then
  let () = 
    Mat_option.feedback
      "Welcome to Frama-C's Pilat invariant generator"
  in 
  let file = Ast.get () in
  let () = Cfg.clearFileCFG file ; Cfg.computeFileCFG file 

  in
  let filename = 
    let fname = Mat_option.Output_C_File.get () in 
    if  fname = "" then file.fileName ^ "_annot.c" else fname
  in
  
  let () = 
    let vis = loop_analyzer () in
    Cil.visitCilFile (vis :> Cil.cilVisitor) file
  in
  let prj = 
    File.create_project_from_visitor 
      "new_pilat_project" 
      (fun p -> new Pilat_visitors.fundec_updater p) 
      
  in
  List.iter
    (function
    |GFun (f,_) -> 
      File.must_recompute_cfg f;
    | _ -> ())
    file.globals;

  Mat_option.debug ~dkey:dkey_time 
    "Time to compute the relations : %f" ! Mat_option.whole_rel_time ;
 
   Mat_option.debug ~dkey:dkey_time ~level:2
    "Invariant generation time : %f\nIntersection time : %f\nNullspace time %f\nEigenvalues : %f\n Char. poly %f" 
     !Mat_option.invar_timer 
     !Mat_option.inter_timer 
     !Mat_option.nullspace_timer
     !Mat_option.ev_timer
     !Mat_option.char_poly_timer;
   
  let cout = open_out filename in
  let fmt = Format.formatter_of_out_channel cout in
  Kernel.Unicode.without_unicode
    (fun () ->
      File.pretty_ast ~prj ~fmt ();
      close_out cout;
      Mat_option.feedback "C file generation      : done\n";
    ) ()


let () = Db.Main.extend run
