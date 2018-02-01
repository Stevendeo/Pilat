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

open Pilat_math

exception Missing_variables
exception Incomplete_base
exception Not_solvable

let dkey_to_mat = Mat_option.register_category "assign:to_mat"
let dkey_lowerizer = Mat_option.register_category "matast:lowerizer" 
let dkey_all_monom = Mat_option.register_category "matast:lowerizer:all_monom" 
let dkey_loop_mat = Mat_option.register_category "matast:loop_mat"
let dkey_base = Mat_option.register_category "matast:base"

module type S = sig 

  (** 1. Utils *)

  module P : Polynomial

  module M : Matrix with type elt = P.c

  module R : Ring with type t = P.c
  (** Takes a monomial and its assignment, returns a matrix and its base. 
      If a base is provided it will complete it and use it for the matrix, else it 
      will create a new base from the affectation.
      Raises Incomplete_base if unconsidered variables are necessary for the matrix.
  *)
  module Var = P.Var
    
  val to_mat : ?base:int P.Monom.Map.t -> P.Monom.t -> P.t -> int P.Monom.Map.t * M.t

  type mat = M.t (** Matrix in which the affectation will be translated *)
  type coef = P.c (** Coefficient of the polynomial and of the matrix *)
  type var = P.v (** Variables used by the polynomial *)

  type monomial = P.Monom.t
  type m_set = P.Monom.Set.t
  type p = P.t
  type t = 
      Assign of Var.t * P.t
    | Assert of Cil_types.exp * body * body (* Todo : do not depend on Frama-C *)
    | Loop of body 
    | Other_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)
                        
  and body = t list

  val pretty_assign : Format.formatter -> t -> unit

  (** A monomial assignment is equivalent to considering a monomial is a variable modified
      by the assignment. *)

  type monom_assign = 
    LinAssign of monomial * p
  | LinAssert of Cil_types.exp * lin_body * lin_body
  | LinLoop of lin_body
  | LinOther_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)
                            
  and lin_body = monom_assign list

  val pretty_linassign : Format.formatter -> monom_assign -> unit

  (** For each variable v of the set, returns the assignment v = v. This is needed when
      a variable doesn't appear on each loop body. *)  
  val basic_assigns : P.Var.Set.t -> body

  (** Returns the list of monomial assignments needed to linearize the loop, and the
      set of all monomials used. Raises Missing_variables if variables not present in the
      set in argument are used as r-values in the body list. *)
  val add_monomial_modifications : 
    P.Var.Set.t -> body -> lin_body * P.Monom.Set.t

  module Imap : Map.S with type key = int

  (** Links each monomial to an integer. Used as base for the matrix *)
  val monomial_base : P.Monom.Set.t -> int P.Monom.Map.t

  (** Reverses the base *)
  val reverse_base : int P.Monom.Map.t -> P.Monom.t Imap.t

  (** Given a base, prints a vector as a polynomial *)
  val print_vec : Format.formatter -> P.Monom.t Imap.t * M.vec -> unit

  (** Transforms a vector to a polynomial *)
  val vec_to_poly : P.Monom.t Imap.t -> M.vec -> P.t

  (** Transforms a list of monomial assignments *)
  val loop_matrix : int P.Monom.Map.t -> monom_assign list -> mat list

end

module Make (M:Matrix) (Poly:Polynomial with type c = M.elt) : 
 S with type P.c = M.elt 
    and type P.v = Poly.v
    and type P.Var.Set.t = Poly.Var.Set.t
 = 
struct 

  module M = M
  module P = Poly
  module R = P.R
  module Var = P.Var
  let to_mat ?(base = P.Monom.Map.empty) (monom_var:P.Monom.t) (p:P.t) : int P.Monom.Map.t * M.t = 
    let base_monom = 
      if P.Monom.Map.is_empty base
      then 
	let poly_base = 
	  P.Monom.Set.add monom_var (P.get_monomials p) in 
	let i = ref (-1) in 
	P.Monom.Set.fold
	  (fun m map ->
	    i := !i + 1;
	    P.Monom.Map.add m !i map
	  )
	  poly_base
	  P.Monom.Map.empty
      else base
	
    in
    
    
    let size_base = (P.Monom.Map.cardinal base_monom) in
    let () = 
      Mat_option.debug ~dkey:dkey_to_mat ~level:2 
	"Base of size %i" size_base;
	P.Monom.Map.iter
	  (fun monom i -> 
	    Mat_option.debug ~dkey:dkey_to_mat ~level:3
	  "%a <-> %i" P.Monom.pretty monom i;
	  ) base_monom
	  
      in
      let index_mvar = P.Monom.Map.find monom_var base_monom in
      let () = 
	Mat_option.debug ~dkey:dkey_to_mat ~level:2 
	  "Index of %a is %i" P.Monom.pretty monom_var index_mvar in
      let mat = M.identity size_base in 
      let () = M.set_coef index_mvar index_mvar mat Poly.R.zero in
              
      let row = P.Monom.Map.find monom_var base_monom in 
      
      let () = 
	P.Monom.Set.iter
	  (fun m -> 
	      let col_monom = 
		try P.Monom.Map.find m base_monom 
		with Not_found -> 
		  let () = 
		    Mat_option.debug ~dkey:dkey_to_mat 
		      "%a not found in base" 
		      P.Monom.pretty m
		  in
		  raise Incomplete_base 
	      in
	      let coef = P.coef p m in
	      M.set_coef row col_monom mat coef
	  )
	  (P.get_monomials p)
	  
      in
      base_monom,mat 
  type coef = P.c
  type var = Poly.v
  type mat = M.t
  type monomial = Poly.Monom.t
  type m_set = Poly.Monom.Set.t
  type p = Poly.t 
  

  type t = 
    Assign of var * p
  | Assert of Cil_types.exp * body * body (* Todo : do not depend on Frama-C *)
  | Loop of body
  | Other_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)

  and body = t list

  let pretty_assign fmt a = 
    let cpt = ref 1 in
    let rec __pretty_assign fmt a = 
    match a with
      Assign (var,p) -> 
	Format.fprintf fmt 
	  "%a = %a"
	  Poly.Var.pretty var
	  Poly.pp_print p
    | Loop bdy -> 
	  Format.fprintf fmt 
	    "-- Body %i"
	    !cpt; 
	  cpt := !cpt + 1;
	  List.iter (__pretty_assign fmt) bdy;
	  Format.fprintf fmt 
	    "-- "
    | Assert (e,b1,b2) -> 
      Format.fprintf fmt 
        "if(%a)\n then{%a}\nelse{%a}"
        Printer.pp_exp e
        (Format.pp_print_list __pretty_assign) b1
        (Format.pp_print_list __pretty_assign) b2
    | Other_stmt s -> Format.fprintf fmt "%a" Printer.pp_stmt s

    in __pretty_assign fmt a
    

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)

type monom_assign = 
  
  LinAssign of P.Monom.t * P.t  
| LinAssert of Cil_types.exp * lin_body * lin_body (* Todo : do not depend on Frama-C *)
| LinLoop of lin_body
| LinOther_stmt of Cil_types.stmt (* Todo : do not depend on Frama-C *)

and lin_body = monom_assign list

let pretty_linassign fmt a = 
  let cpt = ref 1 in

  let rec __pretty_linassign fmt a = 
    match a with
      LinAssign (var,p) -> 
	Format.fprintf fmt 
	  "%a = %a"
	  Poly.Monom.pretty var
	  Poly.pp_print p
    | LinLoop bdy -> 
      Format.fprintf fmt 
        "-- Body %i"
        !cpt;
      cpt := !cpt + 1;
      List.iter (__pretty_linassign fmt) bdy;
      Format.fprintf fmt 
	"-- "
    | LinAssert (e,b1,b2) -> 
      Format.fprintf fmt 
        "if(%a)\n then{%a}\nelse{%a}"
        Printer.pp_exp e
        (Format.pp_print_list __pretty_linassign) b1
        (Format.pp_print_list __pretty_linassign) b2
    | LinOther_stmt s -> Format.fprintf fmt "%a" Printer.pp_stmt s

  in __pretty_linassign fmt a


let all_possible_monomials e_deg_hashtbl =
  let module M_set =P.Monom.Set in
  let max_deg = Mat_option.Degree.get () in
  let () = 
  Mat_option.debug ~dkey:dkey_all_monom ~level:7
    "Hashtbl content : ";
    P.Var.Hashtbl.iter
      (fun v d -> 
	Mat_option.debug ~dkey:dkey_all_monom ~level:7
	  "Variable %a <-> degree %i"
	  P.Var.pretty v
	  d
      ) e_deg_hashtbl in

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
 
let basic_assigns v_set = 
  Var.Set.fold
    (fun v acc ->
      Assign 
	((v, (P.monomial P.R.one [v,1]))):: acc )
    v_set
    []
    
let add_monomial_modifications 
    (var_used : P.Var.Set.t) (p_list:body) : lin_body * P.Monom.Set.t = 
  
  let module M_set = P.Monom.Set in
  let module M_map = P.Monom.Map in
  let l_size = List.length p_list in
  let var_monom_tbl = Var.Hashtbl.create l_size
  in
  
  let effective_degree = Var.Hashtbl.create l_size in
  let num_var_used =  P.Var.Set.cardinal var_used in
  let cst_vars = ref P.Monom.Set.empty in
  (* Registration of the monomials used in the transformation of each var. *)

  let rec reg_monomials a_list = 
    Mat_option.debug ~dkey:dkey_lowerizer ~level:5
      "Size of path : %i"
      (List.length a_list);
    List.iter 
      (fun affect -> 
         match affect with 
         
         Other_stmt _ -> ()
         | Assert (_,b1,b2) -> reg_monomials b1;reg_monomials b2
         | Assign (v,p) -> 
	    Mat_option.debug ~dkey:dkey_lowerizer ~level:5
	      "Assign treated : %a = %a" 
	      Var.pretty v
	      P.pp_print p;
	    if P.Var.Set.mem v var_used
	    then 
              let () = 
                Mat_option.debug ~dkey:dkey_lowerizer ~level:5
                  "Variable %a treated"
                  P.Var.pretty v in
	      let useful_monoms = (P.get_monomials p) in
	      let () = 
		M_set.iter
		  (fun m -> 
		    Mat_option.debug ~dkey:dkey_lowerizer ~level:5
		      "Monomial treated : %a"
		      P.Monom.pretty m;
		    let var_set = 
		      m |> P.to_var_set 
			|> P.Var.Set.of_list  
		    in
		      if var_set 
			|> (P.Var.Set.union var_used) 
			|> P.Var.Set.cardinal <> num_var_used
		      then 
                        let () = (** Error reached *)
                          P.Var.Set.iter
                            (fun v -> 
                               Mat_option.debug ~dkey:dkey_lowerizer
                                 "Possible variable: %a"
                                 Var.pretty v)
                            (P.Var.Set.union var_used var_set)
                        in
                        raise Missing_variables
		      else 
			P.Var.Set.iter
			  (fun v -> 
			    if not (Var.Hashtbl.mem var_monom_tbl v)
			    then 
			    Var.Hashtbl.add var_monom_tbl v 
			      (M_set.singleton 
				 (P.mono_minimal [v,1]))
			  )
			  var_set
			
		      )
		  useful_monoms
	      in
	      let old_bind = 
		try 
	          Var.Hashtbl.find 
		    var_monom_tbl 
		    v 
		with 
		  Not_found -> M_set.empty
	      in 	  
		  
	      Var.Hashtbl.replace var_monom_tbl v (M_set.union old_bind useful_monoms)
            else
              let () = 
                Mat_option.debug ~dkey:dkey_lowerizer ~level:4
                  "Variable %a does not appear in used variable set" 
                  Var.pretty v
              in
              Var.Set.iter
                (fun v -> 
                   Mat_option.debug  ~dkey:dkey_lowerizer ~level:5
                     "%a" Var.pretty v)  var_used
                
                
	| Loop l -> reg_monomials l
      )
      a_list
  in
  let () = reg_monomials p_list in

  Var.Hashtbl.iter
    (fun v _ -> Mat_option.debug ~dkey:dkey_lowerizer ~level:7 
      "Table contains variable %a" Var.pretty v)
    var_monom_tbl;

 
  let compute_effective_degree v = 
    let rec __compute_degree seen_vars v =

      Mat_option.debug ~dkey:dkey_lowerizer ~level:2 
	"Vars seen so far :";
      P.Var.Set.iter 
	(Mat_option.debug ~dkey:dkey_lowerizer ~level:2 "%a" Var.pretty) 
	seen_vars;

      if  P.Var.Hashtbl.mem effective_degree v
      then  P.Var.Hashtbl.find effective_degree v
      else if P.Var.Set.mem v seen_vars 
      then raise Not_solvable
      else 
	begin 
	  let monoms = try Var.Hashtbl.find var_monom_tbl v with Not_found -> M_set.empty in
	  Mat_option.debug ~dkey:dkey_lowerizer ~level:3
	    "Monoms :\n";
	  M_set.iter
	    (fun m -> Mat_option.debug ~dkey:dkey_lowerizer ~level:3 "%a" P.Monom.pretty m) monoms;
	  let deg = 
	    M_set.fold
	      (fun m acc ->
		if P.deg_monom m = 1 then max acc 1 else
		let deg = 
		  List.fold_left
		    (fun acc_deg v2 -> 
		      acc_deg 
		      + 
			(__compute_degree 
			   (Var.Set.add v seen_vars) 
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
	  let () = 
	    Mat_option.debug ~dkey:dkey_lowerizer ~level:3
	      "Effective degree of %a : %i" P.Var.pretty v deg;
	    P.Var.Hashtbl.replace effective_degree v deg
	  in deg
	end
    in
    __compute_degree Var.Set.empty v
  in

  let min_degree = Var.Hashtbl.fold
    (fun v _ acc -> 
      max acc (compute_effective_degree v)
    ) var_monom_tbl 0
  in
  let cst_vars = 
    P.Monom.Set.fold
      (fun m acc -> 
	match P.to_var m with
	  [] -> acc
	| hd :: [] -> P.Var.Set.add hd acc
	| _ -> assert false)
      !cst_vars
      P.Var.Set.empty
  in

    
  if (Mat_option.Degree.get ()) < min_degree
  then Mat_option.abort "The effective degree of the loop is %i, this is the minimal degree for finding invariants. Change the invariant degree to %i." min_degree min_degree;
    
  
  let () = Var.Hashtbl.iter 
    (fun v i -> 
      Mat_option.debug ~dkey:dkey_lowerizer ~level:5
	"Var %a has degree %i"
	Var.pretty v i 
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
	      try Var.Map.find v acc with Not_found -> M_set.empty
	    in
	    Var.Map.add v (M_set.add monom old_bind) acc
	  )
	  map
	  (P.to_var_set monom)
      )
      s
      Var.Map.empty
  in
  let rec linearize affect_list = 
    (List.fold_right
       (fun affect acc  -> 
          match affect with
          Other_stmt s -> LinOther_stmt s :: acc
          | Assert (e,b1,b2) -> LinAssert (e,linearize b1, linearize b2) :: acc
          | Assign (v,poly) -> 
            let monoms_modified = Var.Map.find v modification_map
            in 
            M_set.fold
	       (fun monom acc2 -> 
		 let semi_poly = P.mono_poly R.one monom
		 in
		 let compo = (P.compo semi_poly v poly) in
		 Mat_option.debug ~dkey:dkey_lowerizer ~level:3
		   "%a = %a"
		   P.Monom.pretty monom
		   P.pp_print compo;
		 LinAssign(monom,compo)::acc2
	       )
	       monoms_modified
	       acc
	 | Loop l -> LinLoop (linearize l) :: acc
       )
       affect_list
       [])
  in

  (* Constant variables are treated apart. *)
    
  let cst_assigns = basic_assigns cst_vars in
       linearize (cst_assigns@p_list),s
  
let monomial_base set = 
    let i = ref (-1) in
    P.Monom.Set.fold
      (fun m map -> 
	i := !i + 1;
	Mat_option.debug ~dkey:dkey_base 
	  "%i <-> %a" !i P.Monom.pretty m;
        P.Monom.Map.add m !i map
      )
      set
      P.Monom.Map.empty
  
module Imap = Map.Make(struct type t = int let compare = compare end)

let reverse_base base = 	    
  P.Monom.Map.fold
    (fun monom i intmap -> 
      Mat_option.debug ~level:5 ~dkey:dkey_base "Basis %i : %a" 
	i 
        P.Monom.pretty monom; 
      Imap.add i monom intmap
    )
    base
    Imap.empty 

let print_vec fmt (rev_base,vec) =

  let i = ref (-1) in 
  Array.iter
    (fun c ->
      i := !i + 1; 
      if P.R.equal P.R.zero c
      then () 
      else 
	Format.fprintf fmt
	  "+%a%a" 
	  P.R.pp_print c
	  P.Monom.pretty 
	  (Imap.find !i rev_base)
    ) (M.vec_to_array vec) 

let vec_to_poly rev_base vec = 
  Imap.fold
    (fun i monom acc -> 
      let c = M.get_coef_vec i vec in 
      P.add acc (P.mono_poly c monom)
    )
    rev_base
    P.zero
 let rec matrices_for_a_loop base all_modifs =     
   let matrices = loop_matrix base all_modifs in   
(** In order to consider all the behaviors of the loop, we need to       
    consider as many matrices as they are monomials assigned in a body.       
    An over approximation is to consider all the variables in       
    the loop. *)   
   let number_of_iterations = P.Monom.Map.cardinal base   in   
   List.fold_left     
     (fun acc loop_mat ->       
       let rec matrices_to_succ_powers acc mat i = 	
	 match i with 	  
	   0 -> acc 	
	 | _ -> mat :: (matrices_to_succ_powers acc (M.mul mat loop_mat) (i-1))       
       in       
       matrices_to_succ_powers acc loop_mat number_of_iterations     
     )     
     []     
     matrices 
 and loop_matrix     
    (base : int P.Monom.Map.t)     
    (all_modifs : lin_body)  =     
  (** Multiplies a matrix to each entries of a list. *)   
  let (++) (mat : M.t) (mat_list:M.t list) =     
    List.map (M.mul mat) mat_list     in   
  let mat_size = P.Monom.Map.cardinal base in     
  try       
    List.fold_left 	
      (fun (acc : M.t list) affect -> 	  
	match affect with
           LinOther_stmt _ -> acc
         | LinAssign (v,poly_affect) -> 	      	      
	    let new_matrix = 		
	      (snd
 		 (to_mat
 		    ~base
		    v
		    poly_affect) 		
	      ) in
 	    Mat_option.debug ~dkey:dkey_loop_mat ~level:4
 	      "New matrices for %a =\n %a :"
 	      P.Monom.pretty v
 	      P.pp_print poly_affect; 	      	      
	    List.iter
 	      (fun acc_mat -> 		  
		Mat_option.debug ~dkey:dkey_loop_mat ~level:4 "%a * %a"
 		 M.pp_print new_matrix
 		  M.pp_print acc_mat) acc;
 	    new_matrix ++ acc
        | LinAssert (_,b1,b2) -> 
          let mat_b1 = loop_matrix base b1 and mat_b2 = loop_matrix base b2 in
          List.fold_left
            (fun acc2 m1 -> (m1 ++ acc) @ acc2 ) [] (mat_b1 @ mat_b2)
        | LinLoop body -> 	    
          let matrices_for_each_path = 
            matrices_for_a_loop base body		    
          in 	    
          List.fold_left 	      
            (fun acc2 new_mat -> (new_mat ++ acc) @ acc2) 	      
            acc (* If [], then we don't consider the case where the loop is not taken. *) 	     
            matrices_for_each_path
      ) 	
      [(M.identity mat_size)] 	
      all_modifs     
  with       
    Incomplete_base -> 	
      Mat_option.abort "The matrix base is incomplete, you need to add more variables"   

end
