open Cil_types
open Cil_datatype 
open Pilat_matrix
open Poly_affect 

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
    (p_list:(varinfo * F_poly.t) list) : (F_poly.Monom.t * F_poly.t) list * F_poly.Monom.Set.t = 
  let module M_set = F_poly.Monom.Set in
  let module M_map = F_poly.Monom.Map in
  let l_size = List.length p_list in
  let var_monom_tbl = Varinfo.Hashtbl.create l_size
  in
  
  let effective_degree = Varinfo.Hashtbl.create l_size in

  let () = List.iter (* Registration of the monom used in the transformation of each var *)
    (fun (v,p) -> 

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
      in Varinfo.Hashtbl.replace var_monom_tbl v (M_set.union old_bind useful_monoms))
    p_list 
  in
  
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
  (List.fold_right
    (fun (v,poly) acc  -> 
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
	  (monom,compo)::acc2
	)
	monoms_modified
	acc
    )
    p_list
    []),s
   
(** 2. CIL2Poly  *)

exception Loop_break 

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

let instr_to_poly_assign = function
  | Set (l,e,_) -> begin
    match fst l with 
      Var v -> Some (v,(exp_to_poly e))
    | _ -> assert false end
  | Skip _ -> None
  | _ -> assert false

let register_poly = Stmt.Hashtbl.replace poly_hashtbl   

let stmt_to_poly_assign s : Poly_affect.t option = 
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
	    match instr_to_poly_assign i with 
	      Some p -> register_poly s (Some p); Some p
	    | None -> register_poly s None; None
	  end
      | Loop _ -> assert false
      | Break _ -> raise Loop_break
      | _ -> None
  end

let block_to_poly_lists block : Poly_affect.t list list = 
  let head = List.hd (List.hd block.bstmts).preds (* It must be the entry of the loop *)
  in
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

	  let poly_opt = stmt_to_poly_assign stmt in

	  let future_lists = 
	    List.fold_left
	      (fun acc succ -> (dfs succ) @ acc)
	      []
	      stmt.succs
	  in
	  Mat_option.debug ~dkey:dkey_stmt ~level:3
	    "List of paths : %i" (List.length future_lists) ;
	  match poly_opt with 
	    None -> 
	      Mat_option.debug ~dkey:dkey_stmt ~level:3
		"No polynom generated from this stmt"
	      ;
	      future_lists
	  | Some p ->
	    Mat_option.debug ~dkey:dkey_stmt 
	      "Polynom generated : %a"
	      F_poly.pp_print (snd p);
	    let (++) elt l = List.map (fun li -> elt :: li) l
	    in
	    p ++ future_lists

	with
	  Loop_break -> []
      end 
  in
    
  let res = dfs (List.hd head.succs)
  in
  Mat_option.debug ~dkey:dkey_stmt ~level:5
    "How many paths ? %i" (List.length res); res

(** 3. Matrix from poly *)
 
let lacaml_loop_matrix 
    (base:int F_poly.Monom.Map.t) 
    (all_modifs :(F_poly.Monom.t * F_poly.t) list)  = 
  
  let mat_size = F_poly.Monom.Map.cardinal base in
  List.fold_left
    (fun acc (v,poly_affect) -> 
      let new_matrix = 
	(snd
	   (F_poly.to_lacal_mat 
	      ~base 
	      v 
	      poly_affect)
	) in 
      
      Mat_option.debug ~dkey:dkey_loop_mat ~level:4
	"New matrix for %a = %a :"
        F_poly.Monom.pretty v
	F_poly.pp_print poly_affect;
      
      Mat_option.debug ~dkey:dkey_loop_mat ~level:4 "%a * %a"
	Lacaml_D.pp_mat new_matrix
	Lacaml_D.pp_mat acc;
      
      Lacaml_D.gemm 
	new_matrix
	acc 
    )
    (Lacaml_D.Mat.identity mat_size)
    all_modifs
    
let loop_matrix = 
  lacaml_loop_matrix

let loop_qmat v m = 
  let m = loop_matrix v m in
  Pilat_matrix.lacaml_to_qmat m
