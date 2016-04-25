open Cil_types
open Cil
open Pilat_matrix

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

let print_vec rev_base vec = 

  let i = ref 0 in
  Lacaml_D.Vec.iter
    (fun fl ->
      i := !i + 1;
      if abs_float fl < 1E-10
      then () 
      else 
	Mat_option.debug ~dkey:dkey_stmt
	  "+%f%a" 
	  fl 
	 Poly_affect.F_poly.Monom.pretty 
	  (Imap.find !i rev_base)
    ) vec
    
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

let time = ref 0.
    
(** Visitor *)
    
let loop_analyzer () = 
object(self)
  inherit Visitor.frama_c_inplace
    
  method! vstmt_aux stmt =
    let kf = Extlib.the self#current_kf in
    match stmt.skind with
    | Loop (_,b,_,_,_) -> 
      let t0 = Sys.time() in
      begin (* Loop *)
	
	let () = 	
	  Mat_option.debug ~dkey:dkey_stmt "Loop ided %i studied"
	    stmt.sid in
	
	let polys_opt = 
	try Some (Matrix_ast.block_to_poly_lists b)
	with Matrix_ast.Not_solvable -> None 
	in
	
	match polys_opt with 
	  None -> 
	    Mat_option.debug ~dkey:dkey_stmt "The loop is not solvable"; DoChildren

	| Some poly_lists -> 
	  Mat_option.debug ~dkey:dkey_stmt "The loop is solvable";
	  
	  let varinfos_used = Pilat_visitors.varinfo_registerer b in
	  Mat_option.debug ~dkey:dkey_stmt ~level: 2 "Used varinfos computed";
	  
	  let basic_assigns = 
	    (* In order to compute the transformations for all variables in each 
	       loop, even if a variable doesn't appear on all loops, we need to 
	       add identity assignment *)
	    Cil_datatype.Varinfo.Set.fold
	      (fun v acc -> 
		
	       
		(v, (Poly_affect.F_poly.monomial 1. [v,1])):: acc )
	      varinfos_used
	      []
	      
	    
	  in
	  let affects,bases_for_each_loop = 
	    List.fold_left
	      (fun (acc_affect,acc_base) p_list -> 
		let affect,m_set = 
		  Matrix_ast.add_monomial_modifications 
		    (basic_assigns@p_list) in 
				
		let acc_affect = affect :: acc_affect and  
		    acc_base = Poly_affect.F_poly.Monom.Set.union acc_base m_set in
		(acc_affect,acc_base))
	      ([],Poly_affect.F_poly.Monom.Set.empty)
	      poly_lists
	  in
	  
	  let base = 
	    let i = ref 0 in
	    Poly_affect.F_poly.Monom.Set.fold
	      (fun m map -> 
		i := !i + 1;
		Mat_option.debug ~dkey:dkey_base 
		  "%i <-> %a" !i Poly_affect.F_poly.Monom.pretty m;
	        Poly_affect.F_poly.Monom.Map.add m !i map
	      )
	      bases_for_each_loop
	      Poly_affect.F_poly.Monom.Map.empty
	  in
	  let rev_base = rev_base base in
 
     
	  let first_poly = List.hd affects in 
	  let m1 = Matrix_ast.loop_matrix base first_poly in
	  Mat_option.debug ~dkey:dkey_stmt ~level:2 "First matrix computed";
	  Mat_option.debug ~dkey:dkey_stmt ~level:3 "%a" Lacaml_D.pp_mat m1;


	  let first_invar = Invariant_utils.invariant_computation m1 in
	  Mat_option.debug ~dkey:dkey_stmt ~level:2 "Invar : ";
	  List.iteri
	    (fun i invars -> 
	      let () = 
		Mat_option.debug ~dkey:dkey_stmt
		  "Invariant %i :" (i + 1) in
	      List.iter
		(fun invar ->  
		  print_vec_zarith rev_base invar;
		  Mat_option.debug ~dkey:dkey_stmt "__\n";
		)invars
		
	    ) first_invar;
	  
	  let whole_loop_invar = 
	    List.fold_left
	      (fun acc p_list -> 
		if acc = [] then [] 
		else
		  let m2 = Matrix_ast.loop_matrix base p_list in 	  
		  Mat_option.debug ~dkey:dkey_stmt ~level:3 "New mat : %a" Lacaml_D.pp_mat m2;
 		  let invar = (Invariant_utils.invariant_computation m2)
		  in 

		  Mat_option.debug ~dkey:dkey_stmt ~level:2 "Invar : ";
		  List.iteri
		    (fun i invars -> 
		      let () = 
			Mat_option.debug ~dkey:dkey_stmt
			  "Invariant %i :" (i + 1) in
		      List.iter
			(fun invar ->  
			  print_vec_zarith rev_base invar;
			  Mat_option.debug ~dkey:dkey_stmt "__\n";
			)invars
			
		    ) invar;





		  Invariant_utils.intersection_invariants invar acc
	      )
	      first_invar
	      (List.tl affects)
	  in
	  let whole_loop_invar = 
	  List.map
	    (List.map Invariant_utils.integrate_vec) whole_loop_invar in
	  let () = 
	    Mat_option.debug ~dkey:dkey_stmt
	      "Invariants generated :"
	  in
	  List.iteri
	    (fun i invars -> 
	      let () = 
		Mat_option.debug ~dkey:dkey_stmt
		  "Invariant %i :" (i + 1) in
	      List.iter
		(fun invar ->  
		  print_vec_zarith rev_base invar;
		  Mat_option.debug ~dkey:dkey_stmt "__\n";
		)invars
		
	    )
	    whole_loop_invar;
	  
	  time := Sys.time() -. t0 +. !time;

	  Acsl_gen.add_loop_annots_zarith kf stmt base whole_loop_invar;
	  DoChildren
      end (* Loop *)
    | _ -> DoChildren 
end
     
let run () =  
  if Mat_option.Enabled.get ()
  then
  let () = 
    Mat_option.feedback
      "Welcome to Frama-C Polynomial INvariant Generator"
  in 
  let file = Ast.get () 
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
    "Time to compute the relations : %f" !time ;
 
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
