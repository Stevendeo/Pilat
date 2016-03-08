open Cil_types
open Cil_datatype
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
let loop_poly_hashtbl = Stmt.Hashtbl.create 2

let loop_analyzer () = 
object
  inherit Visitor.frama_c_inplace
    
  val loop_treated = ref Stmt.Set.empty

  method! vstmt_aux s =
 
    match s.skind with
    | Loop (_,b,_,_,_) -> 
      let () = 	
	Mat_option.debug ~dkey:dkey_stmt "Loop ided %i studied"
	  s.sid;
	let res = Matrix_ast.block_to_poly_lists b
	in
	Stmt.Hashtbl.add loop_poly_hashtbl s res;
      in
      DoChildren
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
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file;
  let t0 = Sys.time () in
  
  let () = 
    let vis = loop_analyzer () in
    Cil.visitCilFile (vis :> Cil.cilVisitor) file
  in
  
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
  (*
  Stmt.Hashtbl.iter
    (fun stmt poly_lists -> 
      List.iter
	(fun p_list -> 
	  Mat_option.feedback "Loop %a --\n"
	    Printer.pp_stmt stmt;
	  List.iter
	    (fun (v,poly) -> 
	      Mat_option.feedback
		"Variable %a set to %a\n"
		Cil_datatype.Varinfo.pretty v
		Matrix_ast.F_poly.print poly	      
	    )
	    p_list;
	  let base,mat = Matrix_ast.loop_matrix p_list in 
	  Mat_option.debug ~level:3
	    "Matrix : %a" Lacaml_D.pp_mat mat;

	  let rev_base = 
	  Matrix_ast.F_poly.Monom.Map.fold
	    (fun monom i intmap -> 
	      Mat_option.debug ~level:5 "Basis %i : %a" i Matrix_ast.F_poly.Monom.pretty monom;
	      Imap.add i monom intmap
	    )
	    base 
	  Imap.empty in
	  
	  List.iter
	    (fun vec_l -> 
	      if vec_l = [] then () 
	      else begin
		invs := 1 + !invs;
		e_vec := !e_vec + (List.length vec_l);
	      Mat_option.feedback "New invariants : " ;
	      print_vec rev_base (List.hd vec_l);
	      Mat_option.feedback ")* k =";
	      if (List.tl vec_l) = [] then Mat_option.feedback "0"
	      else 
	      List.iter
		(fun vec -> 
		 Mat_option.feedback "__\n";
		  print_vec rev_base vec
		)
		(List.tl vec_l) end)
	    
	    (Lacaml_utilities.invariant_computation mat)
	  
	)
	poly_lists
    ) loop_poly_hashtbl;

  Mat_option.feedback "%i invariants computed for %i eigenvectors. End." 
    !invs 
    !e_vec;*)
   
  Mat_option.feedback ~dkey:dkey_time
    "Time to compute the relations : %f" 
    (Sys.time () -. t0)
       
let () = Db.Main.extend run
