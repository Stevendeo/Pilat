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

open Pilat_matrix

let dkey_integ = Mat_option.register_category "invar:integrate"
(*let dkey_gen = Mat_option.register_category "invar:generalized"*)
let dkey_inter = Mat_option.register_category "invar:lacaml:inter"
let dkey_redun = Mat_option.register_category "invar:redun"


(* module Int = Datatype.Int  *)

type limit = 
  Convergent of float
| Divergent of float
| Altern
| One
| Zero

type sgn = 
  | Positive
  | Negative
  | Unknown

let pp_limit fmt = function
    Convergent f -> Format.fprintf fmt "Convergent : %f" f
  | Divergent f ->  Format.fprintf fmt "Divergent : %f" f
  | Altern ->  Format.fprintf fmt "Altern"
  | One ->  Format.fprintf fmt "One" 
  | Zero ->  Format.fprintf fmt "Zero"

let pp_sgn fmt = function
    Positive ->  Format.fprintf fmt "Positive"
  | Negative ->  Format.fprintf fmt "Negative"
  | Unknown ->  Format.fprintf fmt "Unknown"

type 'v simp_inv = limit * 'v list

type 'v gen_inv = 'v * 'v

type 'v invariant = 
    Eigenvector of 'v simp_inv
  | Generalized of 'v gen_inv

type q_invar = Pilat_matrix.QMat.vec invariant

module Make (A : Poly_assign.S) =  
struct 
  module Ring = A.P.R
      
  type mat = A.mat
  type invar = A.M.vec invariant

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
  
  (*let mat = Deter_mat.nd_mat_to_d_mat mat in
  *)
    if is_deter
    then
      let evs = A.M.eigenvalues mat in 
      let mat_dim = A.M.get_dim_row mat in
      let matt = (A.M.transpose mat) in
      
      List.fold_left
	(fun acc ev ->
	  let eigen_mat = A.M.sub matt (A.M.scal_mul (A.M.identity mat_dim) ev) in 
	  Eigenvector ((ev_limit ev),(A.M.nullspace eigen_mat)) :: acc
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
	  if ev = Fd.R.one && (List.length eigen_space = 1) then acc else
	  let eigen_space = 
	    List.map
	      undeterminize_vec
	      eigen_space 
	  in
	  Eigenvector ((float_limit (ev|>Fd.P.R.t_to_float)),eigen_space) :: acc
	)
	[] 
	evs


let is_one imap vec = 
  A.Imap.for_all
    (fun i monom -> 
       let coef_vec = A.M.get_coef_vec i vec in 
       not (A.P.Monom.equal monom A.P.empty_monom) || A.P.R.equal coef_vec A.P.R.one)
    imap 

let invar_init_sign imap vec = 
  if is_one imap vec then Positive else Unknown

module GEP = Poly.Make(A.R)(Poly.String_variable)
module MGEP = Pilat_matrix.Make(GEP)

let mul_mat_poly_vec mat poly_vec = 
  let num_vars = A.M.get_dim_col mat in
  let new_vec = MGEP.create_vec num_vars (fun _ -> GEP.zero) in
  let rec coord k =
    if k = num_vars then new_vec else
    let i = ref 0 in 
    let new_coef = 
      A.M.fold_vec
        (fun acc (c:A.R.t) -> 
           let acc = 
             GEP.add acc (GEP.scal_mul c (MGEP.get_coef_vec (!i) poly_vec))
           in let () = i := !i +1 in acc)
        GEP.zero
        (A.M.get_row mat k)
    in
    let () = MGEP.set_coef_vec k new_vec new_coef in
    coord (k+1) 
  in
  coord 0 

let var_vec_define var_vec = 
  let get_var poly = poly |> GEP.get_monomials |> GEP.Monom.Set.choose |> GEP.to_var |> List.hd
  in
  let vars = MGEP.fold_vec 
      (fun acc v -> Poly.String_variable.varname (get_var v) ^ ", " ^ acc) "" var_vec in 
  let removed_comma = String.sub vars 0 ((String.length vars) - 2) in
  removed_comma ^ ": INT;\n"
  
let vec_cstr eq vec_expr vec_obj = 
  let i = ref 0 in
  let prefix,suffix = if eq then "","" else "(NOT ",")" in 
  prefix ^ (MGEP.fold_vec
    (fun acc expr -> 
       "(" ^ GEP.string_cvc expr 
       ^ " = " ^ GEP.string_cvc (MGEP.get_coef_vec !i vec_obj) ^ ") AND " ^ acc
    )
    ""
    vec_expr)^suffix
       
let non_colinear_cstr size vec var_vec = 
  let new_var = Poly.String_variable.new_var () in
  let vec = MGEP.create_vec size (fun i -> GEP.const (A.M.get_coef_vec i vec)) in 
  let col_val = GEP.monomial A.R.one ([new_var,1]) in
  let col_times_var_vec = MGEP.scal_mul_vec var_vec col_val in
    "(NOT (EXISTS (" ^ Poly.String_variable.varname new_var ^ " : REAL) : "
    ^ (vec_cstr true col_times_var_vec vec)^ "))"
 
let send_cstr_to_cvc mat_size c1 c2 old_vecs number_of_vecs_to_find var_vec = 
  let zero_vec = MGEP.create_vec mat_size (fun _ -> GEP.const A.R.zero) in
  let c1cstr = vec_cstr true c1 zero_vec in
  let c2cstr = vec_cstr false c2 zero_vec in 
  let c_old = 
    match old_vecs with 
      [] -> "" 
    | hd:: tl -> 
    List.fold_left 
      (fun acc old_vec -> "(" ^ acc ^ " AND " ^ (non_colinear_cstr mat_size old_vec var_vec) ^ ")")
      (non_colinear_cstr mat_size hd var_vec)
      tl in 
  let whole_cstr = c2cstr ^ " AND " ^ c_old^ ";\n CHECKSAT" ^ c1cstr ^";" in
  let rec find_vecs (cstrs : string) number_of_vecs_to_find res = 
    let () = Mat_option.feedback "Constraint = \n%s" cstrs in
    if number_of_vecs_to_find = 0 then res
    else 
      let cstr = "ASSERT " ^ cstrs ^ "COUNTERMODEL;" in 
      let file_chan = open_out "__tmp_cvc4_query.cvc4" in 
      let fmt = Format.formatter_of_out_channel file_chan in 
      Format.fprintf fmt "%s\n%s" (var_vec_define var_vec) cstr;
      close_out file_chan;
      let _ = Sys.command ("cvc4 -- produce-models __tmp_cvc4_query.cvc4 > __tmp_cvc_4_res.cvc4")
      in 
      (*let file_in = open_in "__tmp_cvc_4_res.cvc4" in 
      *)
      []
  in
  find_vecs whole_cstr number_of_vecs_to_find []
      

let get_new_vecs mat1 mat2 old_vecs (number_of_vecs_to_find : int) = 
  let num_vars = A.M.get_dim_col mat1 in
  let var_vec =
    MGEP.create_vec 
      num_vars 
      (fun _ -> GEP.mono_poly A.R.one (GEP.var_to_monom (Poly.String_variable.new_var ()))) in
  let cstr1 = mul_mat_poly_vec mat1 var_vec in 
  let cstr2 = mul_mat_poly_vec mat2 var_vec in 
  send_cstr_to_cvc num_vars cstr1 cstr2 old_vecs number_of_vecs_to_find var_vec
  
let generalized_invariants mat ev ev_mult = 
    let matt = (A.M.transpose mat) in
    let mat_dim = A.M.get_dim_row mat in
    let mat_minus_ev_id = A.M.sub matt (A.M.scal_mul (A.M.identity mat_dim) ev) in
    let p = mat_dim - ev_mult in 
    Mat_option.feedback "p = %i" p;
    
    let rec get_m m mat_array amlidttn = 
      let rank = mat_dim - List.length (A.M.nullspace amlidttn)
      in
      let () = Mat_option.feedback "Rank of %a is %i." A.M.pp_print amlidttn rank 
      in
      if mat_dim - ev_mult = p then (m,Array.append [|amlidttn|] mat_array) 
      else get_m (m+1) (Array.append [|amlidttn|] mat_array) (A.M.mul mat_minus_ev_id amlidttn)
    in
    let m,mat_array = get_m 1 [|A.M.identity mat_dim|] mat_minus_ev_id in 
    Mat_option.feedback "m = %i" m;
    Mat_option.feedback "Matrices = \n%a" 
      (Format.pp_print_list ~pp_sep:Mat_option.pp_sep A.M.pp_print) (Array.to_list mat_array);

    let rec generalized res k prev_vecs =
      if Array.length mat_array = k+1 then res else 
      let mu = List.length (A.M.nullspace mat_array.(k+1)) - List.length (A.M.nullspace mat_array.(k)) in
      let new_vecs,res =  
        List.fold_left
          (fun (acc_new,acc_res) v -> 
             let new_vec = A.M.sub_vec (A.M.mul_vec matt v) (A.M.scal_mul_vec v ev) in 
             (new_vec :: acc_new, Generalized(v,new_vec) :: acc_res))
          ([],res)
          prev_vecs in
      let additional_vecs = 
        if mu > List.length prev_vecs then 
          (get_new_vecs mat_array.(k) mat_array.(k+1) new_vecs (mu -  List.length prev_vecs)) 
        else [] in 
      generalized res (k+1) (additional_vecs @ new_vecs)
    in
    generalized 
      [] 
      1 
      (get_new_vecs 
         mat_array.(0) 
         mat_array.(1) 
         [] 
         (List.length (A.M.nullspace mat_array.(0)) - List.length (A.M.nullspace mat_array.(1))))
      
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
		 (fun i -> 
		   Mat_option.debug ~dkey:dkey_inter ~level:5 
		     "Trying to get %i th coefficient of vec %a"
		     (i + b1_length)
		     A.M.pp_vec v;
		   try A.M.get_coef_vec (i + b1_length) v
		   with 
		     Invalid_argument s -> 
		       Mat_option.feedback 
			 "%s : Trying to get %i th coefficient of vec %a"
			 s
			 (i + b1_length)
			 A.M.pp_vec v; assert false
			 
		 ) in
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

let intersection_invariants (ll1:invar list) (ll2:invar list) : invar list  =
  let print_vec_list v_list = 
    List.iter
	(fun v -> 
	  Mat_option.debug ~dkey:dkey_inter ~level:5
	    "%a --\n" 
	    A.M.pp_vec v)
      v_list in
 
  List.fold_left
      (fun acc i1 -> 
        match i1 with 
          Generalized _ -> []
        | Eigenvector  (lim1,l1) -> 
          Mat_option.debug ~dkey:dkey_inter ~level:5
            "Intersection of";	
          print_vec_list l1;
          List.fold_left 
            (fun acc2 i2 -> 
               match i2 with 
                 Generalized _ -> []
               | Eigenvector  (lim2,l2) -> 
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
	             let () = 
                       Mat_option.debug ~dkey:dkey_inter ~level:5
		         "Returns ";
		       print_vec_list res in
	               Eigenvector ((join_limits lim1 lim2),res) :: acc2
	      
	  )
	  acc 
	  ll2
      )
      []
      ll1

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

let invar_translate translater (i:'a) =

  match i with
    Eigenvector (lim,inv) -> 
    Eigenvector (lim, (List.map translater inv))
  | Generalized (i1,i2) -> Generalized (translater i1, translater i2)

let zarith_invariant = invar_translate vec_zarith

let to_invar = invar_translate to_vec

let integrate_vec (qvec : QMat.vec) : QMat.vec = 
  let () = 
    Mat_option.debug ~dkey:dkey_integ 
      "Integrating %a"
      QMat.pp_vec qvec in 

  let rec __integ i den = 
    if i = (qvec |> QMat.vec_to_array |> Array.length) then den
    else 
      let den_coef = Q.den (QMat.get_coef_vec i qvec)
      in 
      if Z.equal (Z.rem den den_coef) Z.zero
      then __integ (i+1) den
      else  __integ (i+1) (Z.mul den den_coef) in 
  let res = 
    QMat.scal_mul_vec qvec (Q.(~$$) (__integ 0 Z.one))
  in let () = 
       Mat_option.debug ~dkey:dkey_integ 
         "Result : %a"
         QMat.pp_vec res in res


let integrate_invar (i:invar) = 
  match i with
    Eigenvector (lim,inv) -> 
    Eigenvector (lim, 
                 (List.map 
                    (fun vec -> vec |> vec_zarith |> integrate_vec |> to_vec)) inv)
  | Generalized (inv, vec) -> Generalized (inv |> vec_zarith |> integrate_vec |> to_vec, vec |> vec_zarith |> integrate_vec |> to_vec)
  
let vec_to_poly imap vec =
      A.Imap.fold
	(fun i monom acc -> 
	  let coef_vec = A.M.get_coef_vec i vec in 
	  A.P.add
	    acc
	    (A.P.mono_poly coef_vec monom)
	)
	imap
	A.P.zero
    
let redundant_invariant imap vec vec_list = 
  let vec_poly = vec_to_poly imap vec in
  A.P.is_const vec_poly || (is_one imap vec) ||
  List.exists
    (fun invar -> if invar = vec then false else 
      let vec_invar = vec_to_poly imap invar in
      if A.P.is_const vec_invar then false else
      let () = Mat_option.debug ~dkey:dkey_redun 
	"Does %a divides %a ?" 
	A.P.pp_print vec_invar A.P.pp_print vec_poly
      in
      try 
	ignore (A.P.div vec_poly vec_invar); 
	Mat_option.debug ~dkey:dkey_redun 
	  "Yes, redundant" ;
	true
      with 
	A.P.Not_divisible -> Mat_option.debug ~dkey:dkey_redun 
	  "Not redundant" ; false
    )
    vec_list
end 
