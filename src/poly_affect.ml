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

open Pilat_math
open Cil_types 
open Cil_datatype

exception Incomplete_base
exception Not_solvable


module type S = sig 

  (** 1. Utils *)

  module M : Matrix

  module P : 
    (sig 
      include Polynomial  with type c = M.elt and type v = Varinfo.t
  (** Takes a monomial and its affectation, returns a matrix and its base. 
      If a base is provided it will complete it and use it for the matrix, else it 
      will create a new base from the affectation.
      Raises Incomplete_base if unconsidered variables are necessary for the matrix.
  *)
      val to_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * M.t
     end)

  type mat = M.t(** Matrix in which the affectation will be translated *)

  type coef = P.c (** Coefficient of the polynomial *)
  type var = P.v (** Variables used by the polynomial *)

  type monomial = P.Monom.t
  type m_set = P.Monom.Set.t
  type p = P.t


  type t = 
    Affect of var * p
  | Loop of body list 

  and body = t list

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
  type monom_affect = monomial * p

  (** 2. Ast to matrix translators *)  

  val exp_to_poly : Cil_types.exp -> P.t

  val block_to_poly_lists : P.Var.Set.t-> Cil_types.block -> body list
(** Returns a list of list of polynomial affectations. Each list correspond to a the 
    succession of affectations for each possible path in the loop, while omitting 
    variable absent of the set in argument
    Raises Not_solvable if a statement of the loop is not solvable. *)

  val add_monomial_modifications : 
    body -> monom_affect list * P.Monom.Set.t
(** Returns the list of monomial affectations needed to linearize the loop, and the
    set of all monomials used. *)

end

module Make (M:Matrix) (Poly:Polynomial with type v = Varinfo.t and type c = M.elt) : S = 
struct 

  module M = M
  module P =
  struct 
    include Poly
    let to_mat ?(base = Monom.Map.empty) (monom_var:Monom.t) (p:t) : int Monom.Map.t * M.t = 
      let base_monom = 
	if Monom.Map.is_empty base
	then 
	  let poly_base = 
	    Monom.Set.add monom_var (get_monomials p) in 
	  let i = ref 0 in 
	  Monom.Set.fold
	    (fun m map ->
	      i := !i + 1;
	      Monom.Map.add m !i map
	    )
	    poly_base
	    Monom.Map.empty
	else base
	    
      in
      let size_base = (Monom.Map.cardinal base_monom) in
      let mat = M.zero size_base size_base in
              
      let row = Monom.Map.find monom_var base_monom in 
      
      let () = 
	Monom.Set.iter
	  (fun m -> 
	      let col_monom = 
		try Monom.Map.find m base_monom 
		with Not_found -> raise Incomplete_base
	      in
	      let coef = coef p m in
	      M.set_coef row col_monom mat coef
	  )
	  (get_monomials p)
	  
      in
      base_monom,mat 
  end

  type coef = P.c
  type var = Poly.v
  type mat = M.t
  type monomial = Poly.Monom.t
  type m_set = Poly.Monom.Set.t
  type p = Poly.t 
  

  type t = 
    Affect of var * p
  | Loop of body list 

  and body = t list

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
  type monom_affect = monomial * p

(** 2. Ast to matrix translator *)

let dkey_stmt = Mat_option.register_category "matast:block_analyzer" 
let dkey_lowerizer = Mat_option.register_category "matast:lowerizer" 
let dkey_all_monom = Mat_option.register_category "matast:lowerizer:all_monom" 
let dkey_loop_mat = Mat_option.register_category "matast:loop_mat"


let all_possible_monomials e_deg_hashtbl =
  let module M_set =P.Monom.Set in
  let max_deg = Mat_option.Degree.get () in

  let effective_deg_of_monom monom = 

    let v_list = P.to_var_set monom in 
    List.fold_left
      (fun acc_deg v -> 
	let eff_deg = try P.Var.Hashtbl.find e_deg_hashtbl v with Not_found -> 1
	in
	acc_deg + eff_deg * (P.deg_of_var monom v))
      0
      v_list
  in

  let elevate_monom_if_possible monom v = 
    let d_v = P.Var.Hashtbl.find e_deg_hashtbl v in
    let eff_monom_deg = effective_deg_of_monom monom in
    if d_v + eff_monom_deg > max_deg
    then None
    else Some (P.mono_mul monom (P.mono_minimal [v,1]))
  in
  
  let rec compute_all computed_possible_monomials = 
    Mat_option.debug ~dkey:dkey_all_monom ~level:6 "New iter";
    M_set.iter
      (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:6 "M: %a" P.Monom.pretty m)
      computed_possible_monomials
    ;
    M_set.fold
      (fun monom acc_monoms -> 
	if M_set.mem monom acc_monoms then acc_monoms
	else 
	  let new_monoms = 
	    P.Var.Hashtbl.fold
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
    (P.Var.Hashtbl.fold 
       (fun v _ acc -> 
	 Mat_option.debug ~dkey:dkey_all_monom ~level:4 "%a" P.Var.pretty v;
	 M_set.add (P.mono_minimal [v,1]) acc) 
       e_deg_hashtbl
       M_set.empty)
  in 
  let () = M_set.iter 
    (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:4  "B : %a" P.Monom.pretty m) 
    basis in
  (* delete "(fun m" -> and "m)" for a nice printing bug *)
  let res = compute_all basis
  in
  Mat_option.debug ~dkey:dkey_all_monom ~level:4 "All monomials :";
  M_set.iter
    (fun m -> Mat_option.debug ~dkey:dkey_all_monom ~level:4  "%a" P.Monom.pretty m) res; 
  
  M_set.add (P.mono_minimal []) (M_set.union res basis)
 
  
let add_monomial_modifications 
    (p_list:body) : monom_affect list * P.Monom.Set.t = 

  let module M_set = P.Monom.Set in
  let module M_map = P.Monom.Map in
  let l_size = List.length p_list in
  let var_monom_tbl = P.Var.Hashtbl.create l_size
  in
  
  let effective_degree = P.Var.Hashtbl.create l_size in

  let () = List.iter (* Registration of the monom used in the transformation of each var *)
    (fun affect -> 
      match affect with 
	Affect (v,p) -> 
	  let useful_monoms = 
	    M_set.filter (fun m -> (m |> (P.mono_poly P.R.one) |> P.deg) > 1)
	      (P.get_monomials p)
	  in
	  let old_bind = 
	    try 
	      P.Var.Hashtbl.find 
		var_monom_tbl 
		v 
	    with 
	      Not_found -> M_set.empty
	  in P.Var.Hashtbl.replace var_monom_tbl v (M_set.union old_bind useful_monoms)
      | Loop _ -> assert false
    )
    p_list 
  in
  
  P.Var.Hashtbl.iter
    (fun v _ -> Mat_option.debug ~dkey:dkey_lowerizer ~level:7 
      "Table contains variable %a" P.Var.pretty v)
    var_monom_tbl;

  let compute_effective_degree v = 
    let rec __compute_degree seen_vars v =

      Mat_option.debug ~dkey:dkey_lowerizer ~level:2 
	"Vars seen so far :";
      P.Var.Set.iter 
	(Mat_option.debug ~dkey:dkey_lowerizer ~level:2 "%a" P.Var.pretty) 
	seen_vars;

      if P.Var.Hashtbl.mem effective_degree v
      then P.Var.Hashtbl.find effective_degree v
      else if P.Var.Set.mem v seen_vars 
      then raise Not_solvable
      else 
	begin 
	  let monoms = try P.Var.Hashtbl.find var_monom_tbl v with Not_found -> M_set.empty in
	  Mat_option.debug ~dkey:dkey_lowerizer ~level:3
	    "Monoms :\n";
	  M_set.iter
	    (Mat_option.debug ~dkey:dkey_lowerizer ~level:3 "%a" P.Monom.pretty) monoms;
	  let deg = 
	    M_set.fold
	      (fun m acc -> 
		
		let deg = 
		  List.fold_left
		    (fun acc_deg v2 -> 
		      acc_deg 
		      + 
			(__compute_degree 
			   (P.Var.Set.add v seen_vars) 
			   v2)
		      *
		        P.deg_of_var m v2)
		    0
		    (P.to_var_set m)
		    
		in
		max acc deg
	      )
	      monoms 
	      1
	  in
	  let () = P.Var.Hashtbl.replace effective_degree v deg
	  in deg
	end
    in
    __compute_degree P.Var.Set.empty v
  in

  let min_degree = P.Var.Hashtbl.fold
    (fun v _ acc -> 
      max acc (compute_effective_degree v)
    ) var_monom_tbl 0
  in
  
  if (Mat_option.Degree.get ()) < min_degree
  then Mat_option.abort "The effective degree of the loop is %i, this is the minimal degree for finding invariants. Change the invariant degree to %i." min_degree min_degree;
    
 
  
  let () = P.Var.Hashtbl.iter 
    (fun v i -> 
      Mat_option.debug ~dkey:dkey_lowerizer ~level:5
	"P.Var %a has degree %i"
	P.Var.pretty v i 
    ) effective_degree
  in
 
  let s = all_possible_monomials effective_degree in 
  
  M_set.iter
    (fun m -> Mat_option.debug ~dkey:dkey_lowerizer ~level:6
      "%a" P.Monom.pretty m)
    s; 
  
  let modification_map = 
    M_set.fold
      (fun monom map -> 
	List.fold_left
	  (fun acc v -> 
	    let old_bind = 
	      try P.Var.Map.find v acc with Not_found -> M_set.empty
	    in
	    P.Var.Map.add v (M_set.add monom old_bind) acc
	  )
	  map
	  (P.to_var_set monom)
      )
      s
      P.Var.Map.empty
  in
  (List.fold_right
    (fun affect acc  -> 
      match affect with
	Affect (v,poly) -> 
      
	  let monoms_modified = P.Var.Map.find v modification_map
	  in 
	  
	  M_set.fold
	    (fun monom acc2 -> 
	      let semi_poly = P.mono_poly P.R.one monom
	      in
	      let compo = (P.compo semi_poly v poly) in
	      Mat_option.debug ~dkey:dkey_lowerizer ~level:3
		"%a = %a"
		P.Monom.pretty monom
		P.pp_print compo;
	      (monom,compo)::acc2
	    )
	    monoms_modified
	    acc
      | Loop _ -> assert false
    )
    p_list
    []),s
   
(** 2. CIL2Poly  *)

exception Loop_break 

let poly_hashtbl = Cil_datatype.Stmt.Hashtbl.create 12

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
      P.const (c |> float_of_const |> P.R.float_to_t)
  | Lval (Var v,_) ->
    P.monomial P.R.one [v,1]
  | Lval _ -> assert false
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> assert false
  | UnOp (Neg,e,_) -> 
    P.sub P.zero (exp_to_poly e)
  | UnOp _ -> assert false
  | BinOp (binop,e1,e2,_) -> 
    begin
      match binop with
	PlusA | PlusPI | IndexPI -> P.add (exp_to_poly e1) (exp_to_poly e2)
      | MinusA | MinusPI | MinusPP -> P.sub (exp_to_poly e1) (exp_to_poly e2)
      | Mult -> P.mul (exp_to_poly e1) (exp_to_poly e2)
      | Div -> 
	begin 
	match e2.enode with
	  Const c -> P.mul (exp_to_poly e1) (P.const (1. /.(c |> float_of_const) |> P.R.float_to_t))
	| _ -> Mat_option.abort "The expression %a is a forbidden division." Printer.pp_exp exp
      end	    
      | _ -> assert false
    end
  | CastE (_,e) -> exp_to_poly e
  | _ -> assert false

let instr_to_poly_assign varinfo_used : Cil_types.instr -> t option = 
  function
  | Set (l,e,_) -> begin
    match fst l with 
      Var v -> 
	if P.Var.Set.mem v varinfo_used 
	then Some (Affect (v,(exp_to_poly e)))
	else None 
    | _ -> assert false end
  | Skip _ -> None
  | _ -> assert false

let register_poly = Cil_datatype.Stmt.Hashtbl.replace poly_hashtbl   

let stmt_to_poly_assign varinfo_used s : t option = 
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
	    match instr_to_poly_assign varinfo_used i with 
	      Some p -> register_poly s (Some p); Some p
	    | None -> register_poly s None; None
	  end
      | Cil_types.Loop _ -> Mat_option.abort "Nested loop are not allowed yet."
      | Break _ -> raise Loop_break
      | _ -> None
  end

let block_to_poly_lists varinfo_used block : body list = 
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

	  let poly_opt = stmt_to_poly_assign varinfo_used stmt in

	  let future_lists = 
	    List.fold_left
	      (fun acc succ -> (dfs succ) @ acc)
	      []
	      stmt.succs
	  in
	  Mat_option.debug ~dkey:dkey_stmt ~level:3
	    "List of paths : %i" (List.length future_lists) ;
	  let (++) elt l = List.map (fun li -> elt :: li) l
	  in
	  match poly_opt with 
	    None -> 
	      Mat_option.debug ~dkey:dkey_stmt ~level:3
		"No polynom generated from this stmt"
	      ;
	      future_lists
	  | Some (Affect (_,p) as aff) ->
	    Mat_option.debug ~dkey:dkey_stmt 
	      "Polynom generated : %a"
	      P.pp_print p;

	    aff ++ future_lists
	  | Some (Loop _) -> assert false

	with
	  Loop_break -> []
      end 
  in
    
  let res = dfs (List.hd head.succs)
  in
  Mat_option.debug ~dkey:dkey_stmt ~level:5
    "How many paths ? %i" (List.length res); res



end
