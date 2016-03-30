open Cil_types
open Cil
(*open Logic_const
*)
let dkey_stmt = Mat_option.register_category "main:loop_analyser"
let dkey_time = Mat_option.register_category "main:timer"

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

  Matrix_ast.F_poly.Monom.Map.fold
    (fun monom i intmap -> 
      Mat_option.debug ~level:5 "Basis %i : %a" 
	i 
	Matrix_ast.F_poly.Monom.pretty monom; 
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
	  Matrix_ast.F_poly.Monom.pretty 
	  (Imap.find !i rev_base)
    ) vec
    
let time = ref 0.

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
	
	let res = 
	  try Some (Matrix_ast.block_to_poly_lists b)
	  with Matrix_ast.Not_solvable -> None 
	in
	
	match res with 
	  None -> 
	    Mat_option.debug ~dkey:dkey_stmt "The loop is not solvable"; DoChildren
	| Some poly_lists -> 
	  let first_poly = List.hd poly_lists in 
	  let b1,m1 = Matrix_ast.loop_matrix first_poly in
	  let first_invar = Matrix_utilities.invariant_computation m1 in
	  let whole_loop_invar = 
	    List.fold_left
	      (fun acc p_list -> 
		if acc = [] then [] 
		else
		  let _,m2 = Matrix_ast.loop_matrix p_list in 	  
		  
 		  let invar = (Matrix_utilities.invariant_computation m2)
		  in 
		  Matrix_utilities.intersection_invariants invar acc
	      )
	      first_invar
	      (List.tl poly_lists)
	  in
	  let () = 
	    Mat_option.debug ~dkey:dkey_stmt
	      "Invariants generated :"
	  in
	  let rev_base = rev_base b1 in
	  List.iteri
	    (fun i invars -> 
	      let () = 
		Mat_option.debug ~dkey:dkey_stmt
		  "Invariant %i :" (i + 1) in
	      List.iter
		(fun invar ->  
		  print_vec rev_base invar;
		  Mat_option.debug ~dkey:dkey_stmt "__\n";
		)invars
		  
	    )
	    whole_loop_invar;
	  
	  time := Sys.time() -. t0 +. !time;
	  
	  Acsl_gen.add_loop_annots kf stmt b1 whole_loop_invar;
	  DoChildren
      end (* Loop *)
    | _ -> DoChildren
end

(*
let printer t0 = 
  
  let stmt_to_invarbase_tbl = Stmt.Hashtbl.create 3 in
  
  
  Stmt.Hashtbl.iter
    (fun stmt poly_lists -> 
      let first_poly = List.hd poly_lists in 
      let b1,m1 = Matrix_ast.loop_matrix first_poly in
      let rev_base =  
	Matrix_ast.F_poly.Monom.Map.fold
	  (fun monom i intmap -> 
	    Mat_option.debug ~level:5 "Basis %i : %a" 
	      i 
	      Matrix_ast.F_poly.Monom.pretty monom; 
	    Imap.add i monom intmap
	  )
	  b1
	  Imap.empty in
      let first_invar = Matrix_utilities.invariant_computation m1
      in 
      let stmt_base = 
	List.fold_left
	  (fun acc p_list -> 
	    if acc = [] then [] 
	    else
	    let _,m2 = Matrix_ast.loop_matrix p_list in 	  
	     
 	    let invar = (Matrix_utilities.invariant_computation m2)
	    in 
	    Matrix_utilities.intersection_invariants invar acc
	  )
	  first_invar
	  (List.tl poly_lists)
      in
      Stmt.Hashtbl.add stmt_to_invarbase_tbl stmt (rev_base,stmt_base)
    )
    loop_poly_hashtbl
    ;
  
  Stmt.Hashtbl.iter
    (fun stmt (base,invar_union) -> 
      if invar_union = [] 
      then 
      Mat_option.feedback
	"For loop %a, we generated nothing."
	Printer.pp_stmt stmt
      else
	Mat_option.feedback
	  "For loop %a, we generated :"
	  Printer.pp_stmt stmt;
      
      List.iteri
	(fun i invars -> 
	  Mat_option.feedback "Invariant %i" (i+1)
	  ;
	  List.iter
	    (fun invar ->  
	      print_vec base invar;
	      Mat_option.feedback "__\n";
	    )invars
	)
	invar_union      
    )
    stmt_to_invarbase_tbl;
       
  Mat_option.feedback ~dkey:dkey_time
    "Time to compute the relations : %f" 
    (Sys.time () -. t0)
*)



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
  (*
  let module Imap = Map.Make(struct type t = int let compare = compare end) in
  let print_vec rev_base vec = 
    let i = ref 0 in
    Lacaml_D.Vec.iter
      (fun fl ->
	i := !i + 1;
	if abs_float fl < 1E-10
	then () 
	else 
	  Mat_option.feedback 
	    "+%f%a" 
	    fl 
	    Matrix_ast.F_poly.Monom.pretty 
	    (Imap.find !i rev_base)
      ) vec in
  *)

  Mat_option.debug ~dkey:dkey_time 
    "Time to compute the relations : %f" !time ;
  
  let cout = open_out filename in
  let fmt = Format.formatter_of_out_channel cout in
  Kernel.Unicode.without_unicode
    (fun () ->
      File.pretty_ast ~fmt ();
      close_out cout;
      Mat_option.feedback "C file generation      : done\n";
    ) ()
    
  
let () = Db.Main.extend run
