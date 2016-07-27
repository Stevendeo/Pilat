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

type 'a lim = 
  Convergent of 'a
| Divergent of 'a
| Altern
| One
| Zero

type ('a,'v) inv = 'a lim * 'v list

type q_invar = (Q.t,Pilat_matrix.QMat.vec) inv

module Make (A : Poly_assign.S) =  
struct 
  module Ring = A.P.R

  type limit = Ring.t lim
      
  type invar = (Ring.t,A.M.vec) inv

(** 0. Limit utility *)

let ev_limit (ev:Ring.t) : limit = 
  let ev = if Ring.leq ev Ring.zero then Ring.sub Ring.zero ev else ev
  in
  if ev = Ring.zero then Zero
  else if ev = Ring.one then One  
  else if Ring.leq ev Ring.one then Convergent ev
  else Divergent ev

let join_limits l1 l2 = 
  match l1,l2 with
    One,l | l,One -> l
  | Altern,_ | _,Altern -> Altern
  | Zero,_ | _,Zero -> Zero
  | Convergent a,Convergent b -> Convergent (Ring.mul a b)
  | Divergent a,Divergent b-> Divergent (Ring.mul a b)
  | Convergent _,Divergent _ | Divergent _,Convergent _ -> Altern

let lim_to_string l = 
  match l with
    Zero -> "zero"
  | One -> "one"
  | Altern -> "altern"
  | Convergent _ -> "convergent"
  | Divergent _ -> "divergent"

(** 1. Nullspace computation with time witness *)

let nullspace_computation m = 
  Mat_option.debug ~dkey:dkey_null 
    "Nullspace computation";
  let t = Sys.time () in
  let res = A.M.nullspace m in 
  let () = Mat_option.nullspace_timer := !Mat_option.nullspace_timer +. Sys.time() -. t in
  Mat_option.debug ~dkey:dkey_null
    "Nullspace done"; res
 
(** Invariant computation *)

let invariant_computation mat : invar list = 
  
  let t = Sys.time () in
  let evs = A.M.eigenvalues mat in 
  let mat_dim = A.M.get_dim_row mat in
  let matt = (A.M.transpose mat) in
  let res = 
  List.fold_left
    (fun acc ev -> 
      let eigen_mat = A.M.sub matt (A.M.scal_mul (A.M.identity mat_dim) ev) in 
      ((ev_limit ev),(nullspace_computation eigen_mat)) :: acc
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
     let null_space = nullspace_computation mat
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

let limit_zarith (lim: limit) : Q.t lim = 
  match lim with 
    Zero -> Zero
  | One -> One 
  | Altern -> Altern
  | Convergent ev -> Convergent (ev |> A.P.R.t_to_float |> Q.of_float)
  | Divergent ev -> Divergent(ev |> A.P.R.t_to_float |> Q.of_float)

let vec_zarith (vec:A.M.vec) : QMat.vec = 
  let arr = A.M.vec_to_array vec in 
  (Array.map 
     (fun elt -> (elt |> A.P.R.t_to_float |> Q.of_float))
     arr)
  |> QMat.vec_from_array

let zarith_invariant ((lim,inv):invar) = 
  limit_zarith lim, (List.map vec_zarith inv)

end 

let integrate_vec (qvec : QMat.vec) : QMat.vec = 
  
  let prod_den = ref Q.one in
 
  QMat.create_vec (qvec |> QMat.vec_to_array |> Array.length)
    (fun i -> 
      let elt = QMat.get_coef_vec i qvec in
      let den = Q.den elt in 
      let () = prod_den := (Q.mul !prod_den (Q.of_bigint den)) in
      Q.mul elt !prod_den)  
