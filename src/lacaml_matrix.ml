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

let dkey_null = Mat_option.register_category "lacaml:null"
let dkey_ev = Mat_option.register_category "lacaml:ev"

type t = Lacaml__D.mat
type vec = Lacaml__D.vec
type elt = float

type mat = t

exception Dimension_error of int*int*int*int
    (*
let error m1 m2 = 
  raise (Dimension_error 
	   (Lacaml__D.Mat.dim1 m1, 
	    Lacaml__D.Mat.dim2 m1, 
	    Lacaml__D.Mat.dim1 m2, 
	    Lacaml__D.Mat.dim2 m2))
*)
let zero = Lacaml__D.Mat.make0
let create_mat d1 d2 f = Lacaml__D.Mat.init_rows d1 d2 (fun i j -> f (i-1) (j-1))
let create_vec size f = 
  Lacaml__D.Vec.init
    size
    (fun i -> f (i-1))
let copy_mat m = Lacaml__D.lacpy m
let identity = Lacaml__D.Mat.identity

let of_col_vecs vl = Lacaml__D.Mat.of_col_vecs vl
let transpose m = Lacaml__D.Mat.transpose_copy m
let of_row_vecs vl = vl |> of_col_vecs |> transpose

let cols m = Lacaml__D.Mat.to_col_vecs m
let rows m = m |> transpose |> Lacaml__D.Mat.to_col_vecs

let get_row m i = 
  Lacaml__D.Mat.from_row_vec 
    (Lacaml__D.Vec.of_array (m |> Lacaml__D.Mat.to_array).(i))  

let get_col m i = 
  Lacaml__D.Mat.from_col_vec 
    (Lacaml__D.Vec.init i
       (fun j -> m.{j+1,i+1})
    )
    
let get_col_in_line m i = 
    (Lacaml__D.Vec.init i
       (fun j -> m.{j+1,i+1})
    )

let get_dim_row = Lacaml__D.Mat.dim1
let get_dim_col = Lacaml__D.Mat.dim2

let vec_to_array = Lacaml__D.Vec.to_array
let vec_from_array = Lacaml__D.Vec.of_array

(*let to_array m = Lacaml__D.Mat.to_array m
let from_array m =  Lacaml__D.Mat.of_col_vecs m
*)
let set_coef i j m elt = m.{i+1,j+1} <- elt
let get_coef i j m = m.{i+1,j+1}

let set_coef_vec i v elt = v.{i+1}<- elt
let get_coef_vec i v = v.{i+1}

let fold_vec f acc v = 
  Lacaml__D.Vec.fold f acc v

let map f m = Lacaml__D.Mat.map f m
let mapi _ = assert false

let add m n = 
  let res = copy_mat n in Lacaml__D.Mat.axpy m res ~alpha:1.; res

let add_vec v w = Lacaml__D.Vec.add v w

let sub m n = 
  let res = copy_mat n in Lacaml__D.Mat.axpy m res ~alpha:(-1.); res

let sub_vec v w = Lacaml__D.Vec.sub v w 

let collinear v1 v2 = 
  let size = Lacaml__D.Vec.dim v1 in
  let rec __col (index,coef) = 
    if index >= size then true
    else if (coef *. v1.{index})= v2.{index}
    then __col ((index + 1),coef)
    else false
  in
  let rec good_coef_index idx =
    if idx >= size then (size,0.)
    else if v1.{idx} = 0. then good_coef_index (idx + 1)
    else (idx,(v2.{idx} /. v1.{idx}))
  in
  __col (good_coef_index 0)
    

let transpose m = Lacaml__D.Mat.transpose_copy m

let scal_mul m a = map (fun elt -> a *. elt) m
let scal_mul_vec v a = Lacaml__D.Vec.map (fun elt -> a *. elt) v

let mul m n = Lacaml__D.gemm m n 
let scal_prod v w = Lacaml__D.Vec.sum (Lacaml__D.Vec.mul v w)

let pow _ _ = assert false
let trace _ = assert false
let mul_vec m v = Lacaml__D.gemv m v

(** 2. Eigenvalues of a lacaml matrix *)
(** Computes the eigenvalues of a lacaml matrix.
    This function has several problems, as the lacaml library is
    not precise enough for big matrices. Therefore : 
    
    - Eigenvalues are not guaranteed to be correct for big matrices
    - Complex eigenvalue are ignored 
*)
let eigenvalues matrix = 
  let t = Sys.time () in
  let dimx,dimy = (Lacaml__D.Mat.dim1 matrix),(Lacaml__D.Mat.dim2 matrix)
  in
  
  let vr = (Lacaml__D.Mat.create dimx dimy)
 
  in
  
  let _,a,b,_ = 
    Lacaml__D.geev
      ~vr:(Some vr)
      (copy_mat matrix)
  in 
  let res = ref [] in

  Lacaml__D.Vec.iteri
    (fun i b -> 
      if (b = 0.)
      then 
	let () = Mat_option.feedback ~dkey:dkey_ev "Eigenvalue : %f" a.{i} in
	if not (List.mem a.{i} !res) then res :=  a.{i} :: !res
	else Mat_option.feedback ~dkey:dkey_ev "Eigenvalue %f + i.%f is complex." a.{i} b
      )
    b;

  Mat_option.ev_timer := !Mat_option.ev_timer +. Sys.time () -. t;
  
  !res

(** 3. Nullspace computation for a zarith matrix *)

let approx_float f = 
  (*if abs_float f < 1E-10
  then 0.
  else f *)
  
  let c = ceil f in 
  if c -. f < 1E-10
  then c
  else let fl = floor f in 
       if f -. fl <1E-10
       then fl
       else f
    

let revert_rows mat a b = 
  (*assert dim1 mat = dim2 mat *)
  Mat_option.debug ~dkey:dkey_null ~level:5
    "Switching line %i and %i in %a"
   a b Lacaml__D.pp_mat mat
  ;
  for i=1 to Lacaml__D.Mat.dim2 mat do 
    let tmp = mat.{a,i} in 
    mat.{a,i}<-mat.{b,i};
    mat.{b,i}<- tmp
  done

let mult_row_plus_row mat a b k = (* sets the row a to a + k*b *)
  (* assert dim1 mat = dim2 mat *)

  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing line %i by line %i + %f * line %i of %a"
    a a k b Lacaml__D.pp_mat mat;

  for i=1 to Lacaml__D.Mat.dim2 mat do 
       
    mat.{a,i}<- approx_float (mat.{a,i} +. (k *. mat.{b,i})); 
  done

let div_row mat a k = (* sets the row a to 1/k*a *)  
  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing line %i by 1/%f * line %i in %a"
    a k a Lacaml__D.pp_mat mat;

  for i=1 to Lacaml__D.Mat.dim2 mat do 
    mat.{a,i}<- approx_float (mat.{a,i} /. k); 
  done

let norm_col_down mat col piv = 
  assert (abs_float (mat.{piv,col}) > 1E-10);
  div_row mat piv ( mat.{piv, col});
  
  for i=(piv+1) to Lacaml__D.Mat.dim1 mat do 
    if mat.{i, col} != 0. then
    mult_row_plus_row mat i piv (-1. *. mat.{i, col})
  done
   
let norm_col_up mat col piv = 
  for i=1 to piv - 1 do 
    if mat.{i, col} != 0. then
    mult_row_plus_row mat i piv (-1. *. mat.{i, col})
  done 
exception Done;;

let rref mat = (* Returns the list of the position of the columns that are not pivots *)
  let normalize_mat col piv = 
    for i = piv to Lacaml__D.Mat.dim1 mat do
      if abs_float mat.{i, col} > 1E-10
      then 
	begin 
	  revert_rows mat piv i;
	  norm_col_down mat col piv;
	  norm_col_up mat col piv;
	  raise Done
	end
      
    done
  in
  let piv = ref 1 in
  let no_piv_list = ref [] in
  for col = 1 to Lacaml__D.Mat.dim2 mat do
    try normalize_mat col !piv; 
	no_piv_list := col :: !no_piv_list ;
    with Done -> 
	piv := !piv + 1;
  done
  ;
  !no_piv_list

let insert_val vec elt pos = 
  let dim = Lacaml__D.Vec.dim vec 
  in
  let rec insert vec elt pos =
  if pos > dim
  then () 
  else 
    begin
      let new_elt = vec.{pos} in
      vec.{pos} <- elt;
      insert vec new_elt (pos + 1)
    end
  in
  insert vec elt pos

let lacaml_nullspace_computation mat = 
  
  let no_pivs = List.rev (rref mat) in 

  let dim2 = Lacaml__D.Mat.dim2 mat in

  let num_pivs = dim2 - List.length no_pivs in
  let vecs =
    
    List.fold_right
      (fun no_piv acc -> 
	  if dim2 = Lacaml__D.Mat.dim1 mat
	  then
	    Lacaml__D.Vec.map (fun x -> -1. *. x)
	      (Lacaml__D.Mat.col mat no_piv) :: acc
	  else 
	    let new_vec = Lacaml__D.Vec.create dim2
	    in
	    Lacaml__D.Vec.iteri
	    (fun i f -> 
	      if i <= num_pivs
	      then new_vec.{i} <- -1. *. f
	    )
	      (Lacaml__D.Mat.col mat no_piv)
	    ;
	    new_vec :: acc
	      
	    
      )
      no_pivs
      []
  
  in 
  List.iter2
    (fun vec not_piv ->       
      List.iter
	(fun not_piv2 -> 
	  if not_piv <> not_piv2
	  then 
	    insert_val vec 0. not_piv2
	  else ()
	)
	no_pivs;
      insert_val vec 1. not_piv;

    )
    vecs 
    no_pivs;
  vecs

let nullspace mat = 
    Mat_option.debug ~dkey:dkey_null
    "Nullspace computation";
  let t = Sys.time () in
  let res = lacaml_nullspace_computation mat in 
  let () = Mat_option.nullspace_timer := !Mat_option.nullspace_timer +. Sys.time() -. t in
  Mat_option.debug ~dkey:dkey_null
    "Nullspace done"; 
  List.iter
    (fun v -> 
      Mat_option.debug ~dkey:dkey_null ~level:3
	"%a\n"
	Lacaml__D.pp_vec v
    ) res; 
  res

let pp_print = Lacaml__D.pp_mat
let pp_vec = Lacaml__D.pp_vec

let vec_of_str line = 
    let (num_list : string list) = Str.split (Str.regexp " ") line in 
    let size = List.length num_list in
    let vec = 
      (Array.make size 0.) in
    List.iteri
      (fun i f -> vec.(i) <- float_of_string f) num_list ;
    vec_from_array vec

let of_str (s:string) : t =   
  let line_separator = Str.regexp "\n" in
  let line_list = 
      List.filter
        ((<>) "")
        (Str.split line_separator s) 
  in
  let (vec_list : vec list) = 
    List.map 
      vec_of_str
      line_list
  in
  let arr = Array.of_list vec_list in 
  of_row_vecs arr
