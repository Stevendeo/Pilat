let dkey_null = Mat_option.register_category "lacaml:null"
let dkey_ev = Mat_option.register_category "lacaml:ev"

type t = Lacaml_D.mat
type vec = Lacaml_D.vec

(** 1. Utilities *)
let copy_mat m = m |> Lacaml_D.Mat.to_col_vecs |> Lacaml_D.Mat.of_col_vecs

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

let revert_rows mat a b = 
  (*assert dim1 mat = dim2 mat *)
  for i=1 to Lacaml_D.Mat.dim2 mat do 
    let tmp = mat.{a,i} in 
    mat.{a,i}<-mat.{b,i};
    mat.{b,i}<- tmp
  done

let mult_row_plus_row mat a b k = (* sets the row a to a + k*b *)
  (* assert dim1 mat = dim2 mat *)
  for i=1 to Lacaml_D.Mat.dim2 mat do 
    mat.{a,i}<- mat.{a,i} +. (k *. mat.{b,i}); 
  done

let mult_row mat a k = (* sets the row a to k*a *)
  for i=1 to Lacaml_D.Mat.dim2 mat do 
    mat.{a,i}<-k *. mat.{a,i}; 
  done

let norm_col_down mat col piv = 
  assert (abs_float (mat.{piv,col}) > 1E-20);
  mult_row mat piv (1. /. mat.{piv, col});
  
  for i=(piv+1) to Lacaml_D.Mat.dim1 mat do 
    mult_row_plus_row mat i piv (-1. *. mat.{i, col})
  done
   
let norm_col_up mat col piv = 
  for i=1 to piv - 1 do 
    mult_row_plus_row mat i piv (-1. *. mat.{i, col})
  done 
exception Done;;

let rref mat = (* Returns the list of the position of the columns that are not pivots *)
  let normalize_mat col piv = 
    for i = piv to Lacaml_D.Mat.dim1 mat do
      if abs_float mat.{i, col} > 1E-20
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

let nullspace_computation mat = 
    Mat_option.debug ~dkey:dkey_null
    "Nullspace computation";
  let t = Sys.time () in
  let res = lacaml_nullspace_computation mat in 
  let () = Mat_option.nullspace_timer := !Mat_option.nullspace_timer +. Sys.time() -. t in
  Mat_option.debug ~dkey:dkey_null
    "Nullspace done"; 
  res
