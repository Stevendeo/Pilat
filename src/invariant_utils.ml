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

open Pilat_matrix

let dkey_null = Mat_option.register_category "invar:nullspace"
let dkey_inter = Mat_option.register_category "invar:lacaml:inter"

module Int = Datatype.Int 

type limit = 
  Convergent of float
| Divergent of float
| Altern
| One
| Zero

type 'v inv = limit * 'v list

type q_invar = Pilat_matrix.QMat.vec inv

module type S = 
sig 
  
(** An invariant is an eigenspace, represented by its base with
    a vec list. 
    When an eigenspace is associated to an eigenvalue strictly 
    lower to one, the invariant is convergent 
    (<e,X> < k => <e,MX> <k).
    When it is higher to one, it is divergent
    (<e,X> > k => <e,MX> > k).
*)
  type mat
  type invar
    
  val lim_to_string : limit -> string
    
(** Returns the rational eigenspaces union of the floating matrix 
    as a list of bases. If assignments are non deterministic, the boolean 
    must be set to true *)
  val invariant_computation : bool -> mat -> invar list
    
(** Intersects two union of vectorial spaces. *)
  val intersection_invariants :  invar list -> invar list -> invar list

  val zarith_invariant : invar -> q_invar
  val to_invar : q_invar -> invar
  val integrate_invar : invar -> invar
end

module Make (A : Poly_assign.S) =  
struct 
  module Ring = A.P.R
      
  type mat = A.mat
  type invar = A.M.vec inv

(** 0. Limit utility *)

  let float_limit (ev:float) = 
    let ev = if ev<0. then (-1.)*.ev else ev
    in
    if ev = 0. then Zero
    else if ev = 1. then One  
    else if ev <= 1. then Convergent ev
    else Divergent ev

  let ev_limit (ev:Ring.t) : limit = 
    ev |> Ring.t_to_float |> float_limit


let join_limits l1 l2 = 
  match l1,l2 with
    One,l | l,One -> l
  | Altern,_ | _,Altern -> Altern
  | Zero,_ | _,Zero -> Zero
  | Convergent a,Convergent b -> Convergent (a *. b)
  | Divergent a,Divergent b-> Divergent (a *. b)
  | Convergent _,Divergent _ | Divergent _,Convergent _ -> Altern

let lim_to_string l = 
  match l with
    Zero -> "zero"
  | One -> "one"
  | Altern -> "altern"
  | Convergent _ -> "convergent"
  | Divergent _ -> "divergent"
 
(** Invariant computation *)

module Deter_mat = Assign.Determinizer(A)
module Fd = Assign.Float_deterministic

let undeterminize_vec (vec:Fd.M.vec) = 
  let arr = Fd.M.vec_to_array vec in 
  (Array.map
     (fun elt -> elt |> Fd.P.R.t_to_float |> A.P.R.float_to_t) 
     arr) |> A.M.vec_from_array

let invariant_computation is_deter mat : invar list = 
  
  let t = Sys.time () in
  
  (*let mat = Deter_mat.nd_mat_to_d_mat mat in
  *)
  let res = 
    if is_deter
    then
      let evs = A.M.eigenvalues mat in 
      let mat_dim = A.M.get_dim_row mat in
      let matt = (A.M.transpose mat) in
      
      List.fold_left
	(fun acc ev -> 
	  let eigen_mat = A.M.sub matt (A.M.scal_mul (A.M.identity mat_dim) ev) in 
	  ((ev_limit ev),(A.M.nullspace eigen_mat)) :: acc
	)
	[]
	evs
    else 
      let mat = Deter_mat.nd_mat_to_d_mat mat in
      let evs = Fd.M.eigenvalues mat in 
      let mat_dim = Fd.M.get_dim_row mat in
      let matt = (Fd.M.transpose mat) in
      
      List.fold_left 
	(fun acc ev ->   
	  let eigen_mat = Fd.M.sub matt (Fd.M.scal_mul (Fd.M.identity mat_dim) ev) in 
	  let eigen_space = (Fd.M.nullspace eigen_mat) in 
	  let eigen_space = 
	    List.map
	      undeterminize_vec
	      eigen_space 
	  in
	  ((float_limit (ev|>Fd.P.R.t_to_float)),eigen_space) :: acc
	)
	[] 
	evs
  in
  let () = Mat_option.invar_timer := !Mat_option.invar_timer +. Sys.time () -. t
    in res

let intersection_bases (b1:A.M.vec list) (b2:A.M.vec list) = 
   if b1 = [] || b2 = [] then [||]
   else 
     let mat = A.M.of_col_vecs (Array.of_list (b1@b2))
     in
     let b1 = Array.of_list b1 and b2 = Array.of_list b2 in
     let null_space = A.M.nullspace mat
     in
     let b1_length = Array.length b1 in
     let b2_length = Array.length b2 in
     let () = 
       Mat_option.debug ~dkey:dkey_inter
	 "Matrix : %a\nSize of kernel elements : %i + %i = %i. Size of matrix : %i x %i"
	 A.M.pp_print mat
	 b1_length  b2_length
	 (b1_length + b2_length)
	 (A.M.get_dim_col mat)
	 (A.M.get_dim_row mat);
       List.iter
	 (fun k_vec -> 
	   Mat_option.debug ~dkey:dkey_inter
	     "kernel vector : %a."
	     A.M.pp_vec k_vec
	 ) null_space
     in  
(* http://math.stackexchange.com/questions/189285/calculate-intersection-of-vector-subspace-by-using-gauss-algorithm *)
     let u,n = 
       if b1_length < b2_length
       then 
      (* u = *) (A.M.of_col_vecs b1),
      (* n = (the b1_length first lines of n)*) 
	 List.fold_left
	   (fun acc v -> 
	     let trunc_v = 
	       A.M.create_vec b1_length
		 (fun i -> A.M.get_coef_vec i v)
	     in
	     trunc_v :: acc
	   )
	   []
	   null_space
	   
       else
	 
      (* u = *) (A.M.of_col_vecs b2),(* n = (the b2_length last first lines of n)*) 
	 List.fold_left 
	   (fun acc v ->  
	     let trunc_v = 
	       A.M.create_vec b2_length
		 (fun i -> A.M.get_coef_vec (i + b1_length) v) in
	     trunc_v :: acc	  
	   )
	   []
	   null_space
     in
     if n = [] then [||]
     else 
       let n_mat = A.M.of_col_vecs (Array.of_list n) 
       in
       let () = Mat_option.debug ~dkey:dkey_inter ~level:3 
	 "U*N = %a * %a"
	 A.M.pp_print u
	 A.M.pp_print n_mat in
  
  let mat_base = A.M.mul u n_mat
  in
  A.M.cols mat_base

let intersection_invariants ll1 ll2  =
  let print_vec_list v_list = 
    List.iter
	(fun v -> 
	  Mat_option.debug ~dkey:dkey_inter ~level:5
	    "%a --\n" 
	    A.M.pp_vec v)
      v_list in
 
  List.fold_left
      (fun acc (lim1,l1) -> 
	Mat_option.debug ~dkey:dkey_inter ~level:5
	  "Intersection of";
	
	print_vec_list l1;
	
	
	List.fold_left
	  (fun acc2 (lim2,l2) -> 
	    Mat_option.debug ~dkey:dkey_inter ~level:5
	      "with";
	    print_vec_list l2;
	    let new_base = intersection_bases l1 l2 in
	    if new_base = [||]
	    then
	      begin
		Mat_option.debug ~dkey:dkey_inter ~level:5
		  "Returns nothing !";
	        acc2
	      end
	    else 
	      let res = (Array.to_list new_base) in
	      begin
		Mat_option.debug ~dkey:dkey_inter ~level:5
		  "Returns ";
		print_vec_list res;
		((join_limits lim1 lim2),res) :: acc2
	      end
	  )
	  acc 
	  ll2
      )
      []
      ll1

let intersection_invariants ll1 ll2 = 
  let t = Sys.time () in
  let res = 
    intersection_invariants ll1 ll2
  in
  let () = Mat_option.inter_timer := !Mat_option.inter_timer +. Sys.time () -. t
  in res
(*
let limit_zarith (lim: limit) : Q.t lim = 
  match lim with 
    Zero -> Zero
  | One -> One 
  | Altern -> Altern
  | Convergent ev -> Convergent (ev |> Q.of_float)
  | Divergent ev -> Divergent(ev |> Q.of_float)
*)
let vec_zarith (vec:A.M.vec) : QMat.vec = 
  let arr = A.M.vec_to_array vec in 
  (Array.map 
     (fun elt -> (elt |> A.P.R.t_to_float |> Q.of_float))
     arr)
  |> QMat.vec_from_array

let to_vec (vec:QMat.vec) : A.M.vec = 
  let arr = QMat.vec_to_array vec in 
  (Array.map 
      (fun elt -> 
	elt 
	  |> Qring.t_to_float 
	  |> A.R.float_to_t) 
    arr)
  |> A.M.vec_from_array

let zarith_invariant ((lim,inv):invar) = 
  lim, (List.map vec_zarith inv)

let to_invar ((lim,inv):q_invar) = 
  lim, (List.map to_vec inv)


let integrate_vec (qvec : QMat.vec) : QMat.vec = 
  
  let prod_den = ref Q.one in
 
  QMat.create_vec (qvec |> QMat.vec_to_array |> Array.length)
    (fun i -> 
      let elt = QMat.get_coef_vec i qvec in
      let den = Q.den elt in 
      let () = prod_den := (Q.mul !prod_den (Q.of_bigint den)) in
      Q.mul elt !prod_den)  

let integrate_invar  ((lim,inv):invar) = 
  lim, 
  (List.map 
     (fun vec -> vec |> vec_zarith |> integrate_vec |> to_vec)) inv
  

end 
