open Pilat_matrix

let dkey_inv = Mat_option.register_category "lacaml:inv"
let dkey_ev = Mat_option.register_category "lacaml:ev"
let dkey_inter = Mat_option.register_category "lacaml:inter"
let dkey_zinv = Mat_option.register_category "zarith:inv"
let dkey_zinter = Mat_option.register_category "zarith:inter"
module Int = Datatype.Int 

type vec = Lacaml_D.vec
type mat = Lacaml_D.mat

let copy_mat m = m |> Lacaml_D.Mat.to_col_vecs |> Lacaml_D.Mat.of_col_vecs

(** Eigenvalues of a lacaml matrix *)
let eigen_val matrix = 
  
  let dimx,dimy = (Lacaml_D.Mat.dim1 matrix),(Lacaml_D.Mat.dim2 matrix)
  in
  
  let vr = (Lacaml_D.Mat.create dimx dimy)
 
  in
  
  let _,a,b,_ = 
    Lacaml_D.geev
      ~vr:(Some vr)
      (copy_mat matrix)
  in 
 (* Lacaml_D.Vec.iter
    (fun b -> 
      assert (b = 0.))
    b;
 *)Lacaml_D.Vec.fold
    (fun acc a -> if List.mem a acc  then acc else 
	let () = Mat_option.debug ~dkey:dkey_ev "Ev : %f" a in a::acc)
    []
    a

(** 4. Nullspace computation *)

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

let nullspace_computation mat = 
  
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

(** Invariant computation *)

let invariant_computation mat = 
  let eigen_vals = eigen_val mat in
  let mat_dim = Lacaml_D.Mat.dim1 mat in
    
  List.fold_left
    (fun acc ev -> 
      let new_mat = Lacaml_D.Mat.transpose_copy (copy_mat mat) in 
      let alpha = (-1.) *. ev in 
      let id = (Lacaml_D.Mat.identity mat_dim) in
      let () = (Lacaml_D.Mat.axpy id new_mat ~alpha);
	Mat_option.debug ~dkey:dkey_inv ~level:2 
	  "Computation of the kernel of %a"
	  Lacaml_D.pp_mat new_mat
	
      in
     
      
      (nullspace_computation new_mat) :: acc
    )
    []
    eigen_vals

let invariant_computation_pilat (mat:QMat.t) : QMat.vec list list = 
  let open QMat in
  let eigenvalues = Pilat_matrix.eigenvalues mat in
  let identity = identity (get_dim_row mat) in
  
    Q_Set.fold
      (fun ev acc -> 
	
	let new_mat = sub (transpose mat) (scal_mul identity ev) in 
        (nullspace new_mat) :: acc
      )
      eigenvalues
      []

let intersection_bases b1 b2 = 
  if b1 = [] || b2 = [] then [||]
  else 
   
  let array_b1 = 
    (Array.of_list b1) in
  let array_b2 = 
    (Array.of_list b2) in
    
  let array_vecs = 
    Array.append
      array_b1
      array_b2
  in
  let mat = (* the matrix (b1 b2) *) 
    Lacaml_D.Mat.of_col_vecs array_vecs
    
  in
  let null_space = nullspace_computation mat
  in
  let b1_length = List.length b1 in
  let b2_length = List.length b2 in
  Mat_option.debug ~dkey:dkey_inter
    "Matrix : %a\nSize of kernel elements : %i + %i = %i. Size of matrix : %i x %i"
    Lacaml_D.pp_mat mat
     b1_length  b2_length
    (b1_length + b2_length)
    (Lacaml_D.Mat.dim1 mat)
    (Lacaml_D.Mat.dim2 mat)
  ;
  
  List.iter
    (fun k_vec -> 
	Mat_option.debug ~dkey:dkey_inter
	  "kernel vector : %a. Size : %i"
	  Lacaml_D.pp_vec k_vec
	  (Lacaml_D.Vec.dim k_vec)
    ) null_space;
  
  
  (* http://math.stackexchange.com/questions/189285/calculate-intersection-of-vector-subspace-by-using-gauss-algorithm *)
  
  let u,n = 
    if b1_length < b2_length
    then 
      (* u = *) (Lacaml_D.Mat.of_col_vecs array_b1),
      (* n = (the b1_length first lines of n)*) 
      List.fold_left
	(fun acc v -> 
	  let trunc_v = Lacaml_D.Vec.create b1_length
	  in
	  Lacaml_D.Vec.iteri 
	    (fun i _ -> 
	      trunc_v.{i} <- v.{i};)
	    trunc_v;
	  trunc_v :: acc	  
	)
	[]
	null_space
	
    else
      
      (* u = *) (Lacaml_D.Mat.of_col_vecs array_b2),(* n = (the b2_length last first lines of n)*) 
      List.fold_left
	(fun acc v -> 
	  let trunc_v = Lacaml_D.Vec.create b2_length
	  in
	  Lacaml_D.Vec.iteri 
	    (fun i _ -> 
	      trunc_v.{i} <- v.{i + b1_length};)
	    trunc_v;
	  trunc_v :: acc	  
	)
	[]
	null_space
  in
  if n = [] then [||] else
  
  let n_mat = n |> Array.of_list |> Lacaml_D.Mat.of_col_vecs 
  in
  
  let () = Mat_option.debug ~dkey:dkey_inter ~level:3 
    "U*N = %a * %a"
    Lacaml_D.pp_mat u
    Lacaml_D.pp_mat n_mat in
  
  let mat_base = Lacaml_D.gemm u n_mat
  in
  
  Lacaml_D.Mat.to_col_vecs mat_base

let intersection_invariants ll1 ll2 = 
  (* Takes two union of eigenspaces represented as list of list of vectors,
     and intersects them. *)

  let print_vec_list v_list = 
    List.iter
	(fun v -> 
	  Mat_option.debug ~dkey:dkey_inter ~level:5
	    "%a --\n" 
	    Lacaml_D.pp_vec v)
      v_list in
    List.fold_left
      (fun acc l1 -> 
	Mat_option.debug ~dkey:dkey_inter ~level:5
	  "Intersection of";
	
	print_vec_list l1;
	
	
	List.fold_left
	  (fun acc2 l2 -> 
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
		res :: acc2
	      end
	  )
	  acc 
	  ll2
      )
      []
      ll1
  
(** Intersection bases with zarith *)

let intersection_bases_pilat (b1 : QMat.vec list) b2 : QMat.vec list = 
  if b1 = [] || b2 = [] then []
  else 
    let () = 
      Mat_option.debug ~dkey:dkey_zinter ~level:4
	"Base 1 content : ";
      List.iter
	(fun vec -> 
	  Mat_option.debug ~dkey:dkey_zinter ~level:4
	    "%a\n" QMat.pp_vec vec
	) b1;
      Mat_option.debug ~dkey:dkey_zinter ~level:4
	"Base 2 content : ";
      List.iter
	(fun vec -> 
	  Mat_option.debug ~dkey:dkey_zinter ~level:4
	    "%a\n" QMat.pp_vec vec
	  ) b2 in

  let array_b1 = 
    (Array.of_list b1) in
  let array_b2 = 
    (Array.of_list b2) in
    
  let array_vecs = 
    Array.append
      array_b1
      array_b2
  in
  let mat = (* the matrix (b1 b2) *) 
    QMat.transpose (QMat.from_array array_vecs)
    
  in

  let null_space = QMat.nullspace (QMat.copy_mat mat)
  in
  let b1_length = List.length b1 in
  let b2_length = List.length b2 in
  Mat_option.debug ~dkey:dkey_zinter
    "Matrix :\n%a\nSize of kernel elements : %i + %i = %i. Size of matrix : %i x %i"
    QMat.pp_print mat
     b1_length  b2_length
    (b1_length + b2_length)
    (QMat.get_dim_row mat)
    (QMat.get_dim_col mat)
  ;
  
  List.iter
    (fun k_vec -> 
	Mat_option.debug ~dkey:dkey_zinter
	  "kernel vector : %a."
	  QMat.pp_vec k_vec
    ) null_space;
  
  
  (* http://math.stackexchange.com/questions/189285/calculate-intersection-of-vector-subspace-by-using-gauss-algorithm *)
  if null_space = [] then []
  else 
    let arr_nullspace = Array.of_list null_space in
    let () = 	  
      Mat_option.debug ~dkey:dkey_zinter ~level:4 
	"Arr nullspace =";
      Array.iter
	(fun vec -> Mat_option.debug ~dkey:dkey_zinter ~level:4 "%a" QMat.pp_vec vec)
	arr_nullspace
    in
    let u,n = 
      if b1_length < b2_length
      then 
      (* u = *) QMat.transpose (QMat.from_array array_b1),
      (* n = (the b1_length first lines of n)*) 

      QMat.create_mat (Array.length arr_nullspace) (QMat.get_dim_col mat)
	(fun i j -> 
	  (QMat.vec_to_array arr_nullspace.(i)).(j)
	)
      else
	let () = 
	  Mat_option.debug ~dkey:dkey_zinter ~level:4 
	  "We ommit the %i first lines of the nullspace. Mat n of dimension %i . %i" 
	    b2_length b2_length (QMat.get_dim_col mat - b1_length);
	  
	in
	
      (* u = *) QMat.transpose ((QMat.from_array array_b2)),
	(* n = (the b2_length last first lines of n)*) 
        QMat.create_mat (Array.length arr_nullspace) (QMat.get_dim_col mat  - b1_length)
	  (fun i j -> 
	    Mat_option.debug ~dkey:dkey_zinter ~level:4
	      "Taking coordinates %i,%i+%i" i (j) b1_length;
	    (QMat.vec_to_array arr_nullspace.(i))
	      .(j + b1_length)
	  )
    in  
    
    let n = QMat.transpose n in
    
    let () = Mat_option.debug ~dkey:dkey_zinter ~level:3 
    "U*N = %a * %a"
    QMat.pp_print u QMat.pp_print n in
  
  let mat_base = QMat.transpose (QMat.mul u n)
  in 
  Array.fold_left
    (fun acc arr -> (QMat.vec_from_array arr) :: acc)
    []
    (QMat.to_array mat_base)

  
let intersection_invariants_pilat ll1 ll2 = 
  (* Takes two union of eigenspaces represented as list of list of vectors,
     and intersects them. *)

  let print_vec_list v_list = 
    List.iter
      (fun v -> 
	Mat_option.debug ~dkey:dkey_zinter ~level:5
	  "%a --\n" 
	  QMat.pp_vec v)
      v_list in
  
  List.fold_left
    (fun acc l1 -> 
      Mat_option.debug ~dkey:dkey_inter ~level:5
	"Intersection of";
      
      print_vec_list l1;
      
      
      List.fold_left
	(fun acc2 l2 -> 
	  Mat_option.debug ~dkey:dkey_zinter ~level:5
	    "with";
	  print_vec_list l2;
	  let new_base = intersection_bases_pilat l1 l2 in
	  if new_base = []
	  then
	    begin
	      Mat_option.debug ~dkey:dkey_zinter ~level:5
		"Returns nothing !";
	      acc2
	    end
	  else 
	    let res = new_base in
	    begin
	      Mat_option.debug ~dkey:dkey_zinter ~level:5
		"Returns ";
	      print_vec_list res;
	      res :: acc2
	    end
	)
	acc 
	ll2
    )
    []
    ll1
    
let integrate_vec (vec:QMat.vec) = 
  let array = QMat.vec_to_array vec in
  let coef = 
  Array.fold_left
    (fun acc elt -> 

      let den =  (Q.den elt) in 
      if Z.equal Z.zero (Z.rem acc den)
      then acc 
      else
      
	Z.mul acc den
    )
    Z.one 
    array
  in
  
  Array.map
    (fun elt -> Q.mul (Q.(~$$)coef) elt)
    array
    
    |> QMat.vec_from_array 


