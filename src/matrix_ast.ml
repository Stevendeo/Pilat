open Cil_types
open Poly_affect 
open Cil_datatype

exception Not_solvable

let dkey_stmt = Mat_option.register_category "matast:block_analyzer" 
let dkey_lowerizer = Mat_option.register_category "matast:lowerizer" 
let dkey_all_monom = Mat_option.register_category "matast:lowerizer:all_monom" 
let dkey_loop_mat = Mat_option.register_category "matast:loop_mat"


let all_possible_monomials e_deg_hashtbl =
  let module M_set = F_poly.Monom.Set in
  let max_deg = Mat_option.Degree.get () in

  let effective_deg_of_monom monom = 

    let v_list = F_poly.to_var_set monom in 
    List.fold_left
      (fun acc_deg v -> 
	let eff_deg = try Varinfo.Hashtbl.find e_deg_hashtbl v with Not_found -> 1
	in
	acc_deg + eff_deg * (F_poly.deg_of_var monom v))
      0
      v_list
  in

  let elevate_monom_if_possible monom v = 
    let d_v = Varinfo.Hashtbl.find e_deg_hashtbl v in
    let eff_monom_deg = effective_deg_of_monom monom in
    if d_v + eff_monom_deg > max_deg
    then None
    else Some (F_poly.mono_mul monom (F_poly.mono_minimal [v,1]))
  in
  
  let rec compute_all computed_possible_monomials = 
    Mat_option.debug ~dkey:dkey_all_monom ~level:6 "New iter";
    M_set.iter
      (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:6 "M: %a" F_poly.Monom.pretty m)
      computed_possible_monomials
    ;
    M_set.fold
      (fun monom acc_monoms -> 
	if M_set.mem monom acc_monoms then acc_monoms
	else 
	  let new_monoms = 
	    Varinfo.Hashtbl.fold
	      (fun v _ acc -> 
		match elevate_monom_if_possible monom v with
		  None -> acc
		| Some m -> M_set.add m acc )
	      
	      e_deg_hashtbl
	      M_set.empty
	  in
	  if M_set.is_empty new_monoms 
	  then 
	    acc_monoms 
	  else 
	    M_set.union (M_set.union acc_monoms new_monoms) (compute_all new_monoms)
      )
      computed_possible_monomials
      M_set.empty
  in
  Mat_option.debug ~dkey:dkey_all_monom ~level:4 "Base of monomials : ";
  let basis =
    (Varinfo.Hashtbl.fold 
       (fun v _ acc -> 
	 Mat_option.debug ~dkey:dkey_all_monom ~level:4 "%a" Varinfo.pretty v;
	 M_set.add (F_poly.mono_minimal [v,1]) acc) 
       e_deg_hashtbl
       M_set.empty)
  in 
  let () = M_set.iter 
    (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:4  "B : %a" F_poly.Monom.pretty m) 
    basis in
  (* delete "(fun m" -> and "m)" for a nice printing bug *)
  let res = compute_all basis
  in
  Mat_option.debug ~dkey:dkey_all_monom ~level:4 "All monomials :";
  M_set.iter
    (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:4  "%a" F_poly.Monom.pretty m) res; 
  
  M_set.add (F_poly.mono_minimal []) (M_set.union res basis)
 
  
let add_monomial_modifications 
    (p_list:Poly_affect.body list) : Poly_affect.lin_body list * F_poly.Monom.Set.t = 
  let module M_set = F_poly.Monom.Set in
  let module M_map = F_poly.Monom.Map in
  let l_size = List.length p_list in
  let var_monom_tbl = Varinfo.Hashtbl.create l_size
  in
  
  let effective_degree = Varinfo.Hashtbl.create l_size in
  
  let rec reg_monomials affect_list = 
    List.iter (* Registration of the monomials used in the transformation of each var *)
      (fun affect -> 
	match affect with 
	  Affect (v,p) -> 
	    let useful_monoms = 
	      M_set.filter (fun m -> (m |> (F_poly.mono_poly Ring.one) |> F_poly.deg) > 1)
		(F_poly.get_monomials p)
	    in
	    let old_bind = 
	      try 
		Varinfo.Hashtbl.find 
		  var_monom_tbl 
		  v 
	      with 
		Not_found -> M_set.empty
	    in Varinfo.Hashtbl.replace var_monom_tbl v (M_set.union old_bind useful_monoms)
	| Loop l -> List.iter reg_monomials l
      )
      affect_list 
  in
  let () = List.iter reg_monomials p_list in

  Varinfo.Hashtbl.iter
    (fun v _ -> Mat_option.debug ~dkey:dkey_lowerizer ~level:7 
      "Table contains variable %a with id %i" Varinfo.pretty v v.vid)
    var_monom_tbl;

  let compute_effective_degree v = 
    let rec __compute_degree seen_vars v =

      Mat_option.debug ~dkey:dkey_lowerizer ~level:2 
	"Vars seen so far :";
      Varinfo.Set.iter 
	(Mat_option.debug ~dkey:dkey_lowerizer ~level:2 "%a" Varinfo.pretty) 
	seen_vars;

      if Varinfo.Hashtbl.mem effective_degree v
      then Varinfo.Hashtbl.find effective_degree v
      else if Varinfo.Set.mem v seen_vars 
      then raise Not_solvable
      else 
	begin 
	  let monoms = try Varinfo.Hashtbl.find var_monom_tbl v with Not_found -> M_set.empty in
	  Mat_option.debug ~dkey:dkey_lowerizer ~level:3
	    "Monoms :\n";
	  M_set.iter
	    (Mat_option.debug ~dkey:dkey_lowerizer ~level:3 "%a" F_poly.Monom.pretty) monoms;
	  let deg = 
	    M_set.fold
	      (fun m acc -> 
		
		let deg = 
		  List.fold_left
		    (fun acc_deg v2 -> 
		      acc_deg 
		      + 
			(__compute_degree 
			   (Varinfo.Set.add v seen_vars) 
			   v2)
		      *
		        F_poly.deg_of_var m v2)
		    0
		    (F_poly.to_var_set m)
		    
		in
		max acc deg
	      )
	      monoms 
	      1
	  in
	  let () = Varinfo.Hashtbl.replace effective_degree v deg
	  in deg
	end
    in
    __compute_degree Varinfo.Set.empty v
  in

  let min_degree = Varinfo.Hashtbl.fold
    (fun v _ acc -> 
      max acc (compute_effective_degree v)
    ) var_monom_tbl 0
  in
  
  if (Mat_option.Degree.get ()) < min_degree
  then Mat_option.abort "The effective degree of the loop is %i, this is the minimal degree for finding invariants. Change the invariant degree to %i." min_degree min_degree;
    
 
  let () = Varinfo.Hashtbl.iter 
    (fun v i -> 
      Mat_option.debug ~dkey:dkey_lowerizer ~level:5
	"Varinfo %a of id %i has degree %i"
	Varinfo.pretty v v.vid i 
    ) effective_degree
  in
 
  let s = all_possible_monomials effective_degree in 
  
  M_set.iter
    (fun m -> Mat_option.debug ~dkey:dkey_lowerizer ~level:6
      "%a" F_poly.Monom.pretty m)
    s; 
  
  let modification_map = 
    M_set.fold
      (fun monom map -> 
	List.fold_left
	  (fun acc v -> 
	    let old_bind = 
	      try Varinfo.Map.find v acc with Not_found -> M_set.empty
	    in
	    Varinfo.Map.add v (M_set.add monom old_bind) acc
	  )
	  map
	  (F_poly.to_var_set monom)
      )
      s
      Varinfo.Map.empty
  in
  let rec linearize affect_list = 
    (List.fold_right
       (fun affect acc  -> 
	 match affect with
	   Affect (v,poly) -> 
	     
	     let monoms_modified = Varinfo.Map.find v modification_map
	     in 
	     
	     M_set.fold
	       (fun monom acc2 -> 
		 let semi_poly = F_poly.mono_poly 1. monom
		 in
		 let compo = (F_poly.compo semi_poly v poly) in
		 Mat_option.debug ~dkey:dkey_lowerizer ~level:3
		   "%a = %a"
		   F_poly.Monom.pretty monom
		   F_poly.pp_print compo;
		 LinAffect(monom,compo)::acc2
	       )
	       monoms_modified
	       acc
	 | Loop l -> LinLoop (List.map linearize l) :: acc
       )
       affect_list
       [])
  in
  
  (List.map linearize p_list),s
   
(** 2. CIL2Poly  *)

exception Loop_break of stmt

let poly_hashtbl = Stmt.Hashtbl.create 12

let rec exp_to_poly exp =
  let float_of_const c = 
    match c with
      CInt64 (i,_,_) -> Integer.to_float i
    | CChr c -> Integer.to_float (Cil.charConstToInt c)
    | CReal (f,_,_) -> f
    | _ -> assert false
  in
  match exp.enode with 
    Const c -> 
      F_poly.const (float_of_const c)
  | Lval (Var v,_) ->
    F_poly.monomial 1. [v,1]
  | Lval _ -> assert false
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> assert false
  | UnOp (Neg,e,_) -> 
    F_poly.sub F_poly.zero (exp_to_poly e)
  | UnOp _ -> assert false
  | BinOp (binop,e1,e2,_) -> 
    begin
      match binop with
	PlusA | PlusPI | IndexPI -> F_poly.add (exp_to_poly e1) (exp_to_poly e2)
      | MinusA | MinusPI | MinusPP -> F_poly.sub (exp_to_poly e1) (exp_to_poly e2)
      | Mult -> F_poly.mul (exp_to_poly e1) (exp_to_poly e2)
      | Div -> 
	begin 
	match e2.enode with
	  Const c -> F_poly.mul (exp_to_poly e1) (F_poly.const (1./.(float_of_const c)))
	| _ -> Mat_option.abort "The expression %a is a forbidden division." Printer.pp_exp exp
      end	    
      | _ -> assert false
    end
  | CastE (_,e) -> exp_to_poly e
  | _ -> assert false

let instr_to_poly_assign varinfo_used : Cil_types.instr -> Poly_affect.t option = 
  function
  | Set (l,e,_) -> begin
    match fst l with 
      Var v -> 
	if Cil_datatype.Varinfo.Set.mem v varinfo_used 
	then Some (Affect (v,(exp_to_poly e)))
	else None 
    | _ -> assert false end
  | Skip _ -> None
  | _ -> assert false

let register_poly : Cil_types.stmt -> Poly_affect.t option -> unit = 
  Stmt.Hashtbl.replace poly_hashtbl   

(* Loop stmt -> Loop break *)
let loop_break_tbl = Stmt.Hashtbl.create 3

let rec stmt_to_poly_assign varinfo_used s : Poly_affect.t option = 
  begin
  try 
    Stmt.Hashtbl.find poly_hashtbl s
  with 
    Not_found -> 
      match s.skind with
	Instr i -> 
	  let () = Mat_option.debug ~dkey:dkey_stmt
	    "Instruction"
	  in
	  begin
	    let instr = instr_to_poly_assign varinfo_used i in
	    register_poly s instr; instr

	  end
      | Cil_types.Loop (_,b,_,_,_) -> 
	let () = 
	  Mat_option.debug ~dkey:dkey_stmt
	    "Inner loop"
	  in
	let res = 
	    Some(Loop (block_to_poly_lists varinfo_used b))
	in
	register_poly s res; res

      | Break _ -> raise (Loop_break s)
      | _ -> None
  end

and block_to_poly_lists varinfo_used block : Poly_affect.body list = 

  (* The first statement of a block loop is the entry of the loop. *)
  let first_block_stmt = (List.hd block.bstmts) in
  let () = Mat_option.debug ~dkey:dkey_stmt ~level:2
      "First stmt of the loop : %a." 
      Stmt.pretty first_block_stmt;
    List.iter
      (fun s -> 
	Mat_option.debug ~dkey:dkey_stmt ~level:3
	  "Its successors : %a" 
	  Stmt.pretty s;)
      first_block_stmt.succs
  in
  
  let head = List.hd first_block_stmt.preds (* It must be the entry of the loop *)
  in
  Mat_option.debug ~dkey:dkey_stmt ~level:2
      "Loop head : %a" 
      Stmt.pretty head
    ;
  let rec dfs stmt = 
    Mat_option.debug ~dkey:dkey_stmt ~level:2
      "Stmt %a studied" 
      Stmt.pretty stmt
    ;
    if Stmt.equal stmt head
    then 
    begin  
      Mat_option.debug ~dkey:dkey_stmt ~level:3
	"Stmt already seen : loop." 
      ;
      [[]] 
    end
    else 
      begin
	Mat_option.debug ~dkey:dkey_stmt ~level:3
	  "Stmt never seen" 
	;
		
	try

	  let poly_list = stmt_to_poly_assign varinfo_used stmt in
	  let next_stmt =  
	    match stmt.skind with
	      Cil_types.Loop (_,_,_,_,break) -> 
		begin
		  try (Extlib.the break) with
		    Invalid_argument _ (* Extlib.the *) -> 
		      let () =  
			Mat_option.feedback "CFG error, loop are not prepared. Use of memoizers"
		      in
		      try 
			Stmt.Hashtbl.find loop_break_tbl stmt 
		      with 
			Not_found -> 
			  Mat_option.abort "Nested loop that does not end not supported"
			
		end
	    | _ -> stmt in
		
	  let future_lists = 
	    List.fold_left
	      (fun acc succ -> (dfs succ) @ acc)
	      []
	      next_stmt.succs
	  in
	  Mat_option.debug ~dkey:dkey_stmt ~level:3
	    "List of paths : %i" (List.length future_lists) ;
	  let (++) elt l = List.map (fun li -> elt :: li) l (* Adds an element to all the lists *)
	  in
	  	  
	  match poly_list with 
	    None -> 
	      Mat_option.debug ~dkey:dkey_stmt ~level:3
		"No polynom generated from stmt %a" Printer.pp_stmt stmt	      ;
	      future_lists
	  | Some (Affect (_,p) as aff) ->
	    Mat_option.debug ~dkey:dkey_stmt 
	      "Polynom generated : %a"
	      F_poly.pp_print p;

	    aff ++ future_lists
	  
	  | Some (Poly_affect.Loop _ as loop) -> loop ++ future_lists
	    

	with
	  Loop_break b -> 
	    let () = Stmt.Hashtbl.add loop_break_tbl head b in []
      end 
  in
    
  let res = dfs (List.hd head.succs)
  in
  Mat_option.debug ~dkey:dkey_stmt ~level:5
    "How many paths ? %i" (List.length res); res

(** 3. Matrix from poly *)
 
let rec matrices_for_a_loop base all_modifs = 
  
  let matrices = lacaml_loop_matrix base all_modifs in

  (** In order to consider all the behaviors of the loop, we need to 
      consider as many matrices as they are monomials assigned in a body. 
      An over approximation is to consider all the variables in 
      the loop. *)
  let number_of_iterations = F_poly.Monom.Map.cardinal base 
  in
  List.fold_left
    (fun acc loop_mat -> 
      let rec matrices_to_succ_powers acc mat i = 
	match i with
	  0 -> acc
	| _ -> mat :: (matrices_to_succ_powers acc (Lacaml_D.gemm mat loop_mat) (i-1))
      in
      matrices_to_succ_powers acc loop_mat number_of_iterations
    )
    []
    matrices

and lacaml_loop_matrix 
    (base : int F_poly.Monom.Map.t) 
    (all_modifs : Poly_affect.lin_body)  = 
  
  (** Multiplies a matrix to each entries of a list. *)
  let (++) (mat : Lacaml_D.mat) (mat_list:Lacaml_D.mat list) = 
    List.map (Lacaml_D.gemm mat) mat_list 
  
  in 
  let mat_size = F_poly.Monom.Map.cardinal base in
    try 
      List.fold_left
	(fun (acc : Lacaml_D.mat list) affect -> 
	  match affect with
	    LinAffect (v,poly_affect) ->
	      
	      let new_matrix = 
		(snd
		   (F_poly.to_lacal_mat 
		      ~base 
		      v 
		      poly_affect)
		) in 
	      
	      Mat_option.debug ~dkey:dkey_loop_mat ~level:4
		"New matrices for %a = %a :"
		F_poly.Monom.pretty v
		F_poly.pp_print poly_affect;
	      
	      List.iter
		(fun acc_mat -> 
		  Mat_option.debug ~dkey:dkey_loop_mat ~level:4 "%a * %a"
		    Lacaml_D.pp_mat new_matrix
		    Lacaml_D.pp_mat acc_mat) acc;
	      
	      new_matrix ++ acc 
	  | LinLoop b_list -> 
	    let matrices_for_each_path = 
	      List.fold_left
		(fun acc body -> (matrices_for_a_loop base body) @ acc)
		[]
		b_list
		
	    in 
	    List.fold_left
	      (fun acc2 new_mat -> (new_mat ++ acc) @ acc2
	      )
	      acc (* If [], then we don't consider the case where the loop is not taken. *)
	      matrices_for_each_path
	      
										    
	)
	[(Lacaml_D.Mat.identity mat_size)]
	all_modifs
    with
      Poly_affect.Incomplete_base -> 
	Mat_option.abort "The matrix base is incomplete, you need to add more variables"
  

let loop_matrix = 
  lacaml_loop_matrix

let loop_qmat v m = 
  let m = loop_matrix v m in
  List.map (Pilat_matrix.lacaml_to_qmat) m
