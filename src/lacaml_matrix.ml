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

let dkey_null = Mat_option.register_category "lacaml:null"
let dkey_ev = Mat_option.register_category "lacaml:ev"

type t = Lacaml_D.mat
type vec = Lacaml_D.vec
type elt = float

type mat = t

exception Dimension_error of int*int*int*int
    
let error m1 m2 = 
  raise (Dimension_error 
	   (Lacaml_D.Mat.dim1 m1, 
	    Lacaml_D.Mat.dim2 m1, 
	    Lacaml_D.Mat.dim1 m2, 
	    Lacaml_D.Mat.dim2 m2))

let zero = Lacaml_D.Mat.make0
let create_mat = Lacaml_D.Mat.init_rows
let create_vec = Lacaml_D.Vec.init
let copy_mat m = Lacaml_D.lacpy m
let identity = Lacaml_D.Mat.identity

let get_row m i = 
  Lacaml_D.Mat.from_row_vec 
    (Lacaml_D.Vec.of_array (m |> Lacaml_D.Mat.to_array).(i))  

let get_col m i = 
  Lacaml_D.Mat.from_col_vec 
    (Lacaml_D.Vec.init i
       (fun j -> m.{j+1,i+1})
    )
    
let get_col_in_line m i = 
    (Lacaml_D.Vec.init i
       (fun j -> m.{j+1,i+1})
    )

let get_dim_row = Lacaml_D.Mat.dim1
let get_dim_col = Lacaml_D.Mat.dim2

let vec_to_array = Lacaml_D.Vec.to_array
let vec_from_array = Lacaml_D.Vec.of_array

let to_array m = Lacaml_D.Mat.to_array m
let from_array m =  Lacaml_D.Mat.of_col_vecs m

let set_coef i j m elt = m.{i+1,j+1} <- elt
let get_coef i j m = m.{i+1,j+1}

let set_coef_vec i v elt = v.{i}<- elt
let get_coef_vec i v = v.{i}

let fold_vec f acc v = 
  Lacaml_D.Vec.fold f acc v

let map f m = Lacaml_D.Mat.map f m
let mapi = assert false

let add m n = 
  let res = copy_mat n in Lacaml_D.Mat.axpy m res ~alpha:1.; res

let add_vec v w = Lacaml_D.Vec.add v w

let sub m n = 
  let res = copy_mat n in Lacaml_D.Mat.axpy m res ~alpha:(-1.); res

let sub_vec v w = Lacaml_D.Vec.sub v w 

let transpose m = Lacaml_D.Mat.transpose_copy m

let scal_mul m a = map (fun elt -> a *. elt) m
let scal_mul_vec v a = Lacaml_D.Vec.map (fun elt -> a *. elt) v

let mul m n = Lacaml_D.gemm m n 
let scal_prod v w = Lacaml_D.Vec.sum (Lacaml_D.Vec.mul v w)

let pow = assert false
let trace = assert false
let mul_vec m v = Lacaml_D.gemv m v

(** 2. Eigenvalues of a lacaml matrix *)
let eigen_val matrix = 
  let t = Sys.time () in
  let dimx,dimy = (Lacaml_D.Mat.dim1 matrix),(Lacaml_D.Mat.dim2 matrix)
  in
  
  let vr = (Lacaml_D.Mat.create dimx dimy)
 
  in
  
  let _,a,b,_ = 
    Lacaml_D.geev
      ~vr:(Some vr)
      (copy_mat matrix)
  in 
  let res = ref [] in

  Lacaml_D.Vec.iteri
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
   a b Lacaml_D.pp_mat mat
  ;
  for i=1 to Lacaml_D.Mat.dim2 mat do 
    let tmp = mat.{a,i} in 
    mat.{a,i}<-mat.{b,i};
    mat.{b,i}<- tmp
  done

let mult_row_plus_row mat a b k = (* sets the row a to a + k*b *)
  (* assert dim1 mat = dim2 mat *)

  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing line %i by line %i + %f * line %i of %a"
    a a k b Lacaml_D.pp_mat mat;

  for i=1 to Lacaml_D.Mat.dim2 mat do 
       
    mat.{a,i}<- approx_float (mat.{a,i} +. (k *. mat.{b,i})); 
  done

let div_row mat a k = (* sets the row a to 1/k*a *)  
  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing line %i by 1/%f * line %i in %a"
    a k a Lacaml_D.pp_mat mat;

  for i=1 to Lacaml_D.Mat.dim2 mat do 
    mat.{a,i}<- approx_float (mat.{a,i} /. k); 
  done

let norm_col_down mat col piv = 
  assert (abs_float (mat.{piv,col}) > 1E-10);
  div_row mat piv ( mat.{piv, col});
  
  for i=(piv+1) to Lacaml_D.Mat.dim1 mat do 
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
    for i = piv to Lacaml_D.Mat.dim1 mat do
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
  for col = 1 to Lacaml_D.Mat.dim2 mat do
    try normalize_mat col !piv; 
	no_piv_list := col :: !no_piv_list ;
    with Done -> 
	piv := !piv + 1;
  done
  ;
  !no_piv_list

let insert_val vec elt pos = 
  let dim = Lacaml_D.Vec.dim vec 
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

  let dim2 = Lacaml_D.Mat.dim2 mat in

  let num_pivs = dim2 - List.length no_pivs in
  let vecs =
    
    List.fold_right
      (fun no_piv acc -> 
	  if dim2 = Lacaml_D.Mat.dim1 mat
	  then
	    Lacaml_D.Vec.map (fun x -> -1. *. x)
	      (Lacaml_D.Mat.col mat no_piv) :: acc
	  else 
	    let new_vec = Lacaml_D.Vec.create dim2
	    in
	    Lacaml_D.Vec.iteri
	    (fun i f -> 
	      if i <= num_pivs
	      then new_vec.{i} <- -1. *. f
	    )
	      (Lacaml_D.Mat.col mat no_piv)
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
	Lacaml_D.pp_vec v
    ) res; 
  res

let pp_print = Lacaml_D.pp_mat
let pp_vec = Lacaml_D.pp_vec
