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

let dkey_ev = Mat_option.register_category "pilat_matrix:eigenvalue"
let dkey_null = Mat_option.register_category "pilat_matrix:nullspace"

(** 1. Matrix functor *)

module Make (F:Ring) : Matrix with type elt = F.t = 
struct
  
  type elt = F.t
  type vec = elt array
  type mat = vec array
  type t = 
    {rows : int;
     cols : int;
     m : mat 
    }
  exception Dimension_error of int*int*int*int
  
  let error m1 m2 = raise (Dimension_error (m1.rows,m1.cols,m2.rows,m2.cols))
      
  let error_vec v1 v2 =  raise (Dimension_error (1,Array.length v1,1,Array.length v2))
  (* 1. Matrix creation *)

  let zero rows cols : t = 
    {
      rows = rows;
      cols = cols;
      m = Array.make_matrix rows cols F.zero
    } 

  let create_mat r c (f: int -> int -> elt) =  
    let m =     
      Array.init r
	(fun i -> 
	  Array.init c (f i)) in
    { rows = r;
      cols = c;
      m = m
    }
    
  let copy_mat mat = 
    create_mat mat.rows mat.cols (fun i j -> mat.m.(i).(j))

  
  let identity n = create_mat n n (fun i j -> if i = j then F.one else F.zero)

  (* 2. Getters and setters *)

  let get_row mat i = 
    { rows = 1; cols = mat.cols; 
      m = [|mat.m.(i)|]
    }
      
  let get_col mat i = 
    { rows = mat.rows; cols = 1;
      m = 
	Array.init mat.rows 
	  (fun row -> [|mat.m.(row).(i)|])
    }
  let get_col_in_line mat i = 
    Array.init mat.rows
      (fun row -> mat.m.(row).(i))

  let get_dim_row mat = mat.rows

  let get_dim_col mat = mat.cols

  let set_coef i j mat elt = mat.m.(i).(j) <- elt

  let get_coef i j mat = mat.m.(i).(j)

  let vec_from_array v = v

  let vec_to_array v = v

  let to_array mat = mat.m
    
  let from_array array =
    if array = [||]
    then {rows = 0; cols = 0; m = array}
    else
      
      let rows = Array.length array in 
      let cols = Array.length array.(0) in 
      Array.iter
	(fun row -> 
	  if Array.length row <> cols
	  then raise (Dimension_error (cols,rows,cols,Array.length row))
	  else ())
	array
      ;
      {rows = rows; cols = cols; m = array}
  (* 3. Iterators  *)

  let mapi (f : int -> int -> elt -> elt) m = 
    {rows = m.rows ; cols = m.cols ; 
     m = 
      Array.mapi
      (fun i vi ->
	Array.mapi
	  (f i)
	  vi
      )
      m.m
    }

  let map (f : elt -> elt) = mapi (fun _ _ -> f) 

(* 4. Matrix operations *)

  let add m1 m2 = 
    if m1.rows <> m2.rows || m1.cols <> m2.cols
    then error m1 m2
    else
    {rows = m1.rows; cols = m1.cols;
     m = 
	Array.mapi
	  (fun i a1 -> 
	    Array.mapi
	      (fun j c1 -> F.add c1 m2.m.(i).(j)) 
	      a1
	  )
	  m1.m
    } 

  let add_vec v1 v2 =    
    if Array.length v1 <> Array.length v2
    then error_vec v1 v2 
    else
      Array.mapi
	(fun i elt -> F.add elt v2.(i)) v1
      
      
  let sub m1 m2 = 
    if m1.rows <> m2.rows || m1.cols <> m2.cols
    then error m1 m2
    else
    {rows = m1.rows; cols = m1.cols;
     m = 
	Array.mapi
	  (fun i a1 -> 
	    Array.mapi
	      (fun j c1 -> F.sub c1 m2.m.(i).(j)) 
	      a1
	  )
	  m1.m
    }

 let sub_vec v1 v2 =    
    if Array.length v1 <> Array.length v2
    then error_vec v1 v2 
    else
      Array.mapi
	(fun i elt -> F.sub elt v2.(i)) v1
      
  let transpose mat = 
    let d1 = Array.length mat.m in
    if d1 = 0 then mat
    else 
      let d2 = Array.length mat.m.(0) in 
      create_mat d2 d1 (fun i j -> mat.m.(j).(i))

  let scal_mul m k = map (F.mul k) m

  let scal_mul_vec m k = Array.map (F.mul k) m 

  let mul m1 m2 = 
    if m1.cols <> m2.rows
    then error m1 m2
    else
    let scal i j = 
    (*  Format.print_int i;
      Format.print_int j;
    *)let _,res = 
	Array.fold_left
	  (fun (cpt,acc) elt -> 

	  (cpt + 1, F.add acc (F.mul elt m2.m.(cpt).(j)))
	)
	(0,F.zero)
	m1.m.(i)
      in res
    in
      
    create_mat m1.rows m2.cols scal
 
  let scal_prod v1 v2 =       
    if Array.length v1 <> Array.length v2
    then error_vec v1 v2 else
      let res = ref F.zero in
      let () = 
      Array.iteri
	(fun i elt -> res := F.add !res (F.mul elt v2.(i)) ) v1
      in !res

  let rec pow mat n = 
    match n with 
      0 -> identity n
    | 1 -> mat
    | _ -> 
      if n mod 2 = 0
      then pow (mul mat mat) (n/2)
      else mul mat (pow (mul mat mat) ((n-1)/2))

  let trace mat = 
    snd (Array.fold_left
      (fun (cpt,acc) vec -> 
	((cpt + 1), F.add acc vec.(cpt)))
      (0,F.zero)
      mat.m
    )

  let mul_vec mat vec = 
    if mat.cols <> Array.length vec
    then raise (Dimension_error (mat.rows,mat.cols,Array.length vec,1))
    else
      let vecs_array = to_array mat in 
      Array.map
	(fun line -> scal_prod line vec)
	vecs_array

let pp_vec fmt v =   

  Format.fprintf fmt "(";
  Array.iter
    (fun elt -> 
      Format.fprintf fmt "%a ," F.pp_print elt)
    v;
  Format.fprintf fmt ")\n"

let pp_print fmt mat = 
  
  Format.fprintf fmt "(";  

  Array.iter
    (fun vec -> 
      (pp_vec fmt vec)
    )
    mat.m;
  Format.fprintf fmt ")\n" 

(** 3. Nullspace computation *)

let revert_rows mat a b = 
  (*assert dim1 mat = dim2 mat *)

  Mat_option.debug ~dkey:dkey_null ~level:5
    "Switching %a and %a"
    pp_vec mat.m.(a) pp_vec mat.m.(b)
  ;

  let tmp = mat.m.(a) in
  mat.m.(a) <- mat.m.(b);
  mat.m.(b) <- tmp

let mult_row_plus_row mat a b k = (* sets the row a to a + k*b *)
  (* assert dim1 mat = dim2 mat *)

  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing %a by %a + %a * %a"
    pp_vec mat.m.(a) 
    pp_vec mat.m.(a) F.pp_print k pp_vec mat.m.(b)
  ;

   mat.m.(a) <-
  Array.mapi
    (fun i elt -> F.add elt (F.mul k mat.m.(b).(i)))
    mat.m.(a)

let mult_row mat a k = (* sets the row a to k*a *)
  Mat_option.debug ~dkey:dkey_null ~level:5
    "Replacing %a by %a * %a"
    pp_vec mat.m.(a) F.pp_print k pp_vec mat.m.(a);

  mat.m.(a) <-
  Array.map 
    (F.mul k)
    mat.m.(a)  

let mone = F.sub F.zero F.one

let norm_col mat col piv = 
  mult_row mat piv (F.div F.one mat.m.(piv).(col));
  
  for i=0 to mat.rows - 1 do 
    if i = piv then ()
    else 
      let m_i_col = mat.m.(i).(col) in
      
      mult_row_plus_row mat i piv (F.mul mone m_i_col) ;
  done 
   
exception Done;;

let rref mat = (* Returns the list of the position of the columns that are not pivots *)
  let normalize_mat col piv = 
    for i = piv to mat.rows - 1 do
      if not (F.equal mat.m.(i).(col) F.zero) then
      begin 
	revert_rows mat piv i;
	norm_col mat col piv;
	raise Done
      end
    done
  in
  let piv = ref 0 in
  let no_piv_list = ref [] in
  for col = 0 to mat.cols - 1 do
    try normalize_mat col !piv; 
	no_piv_list := col :: !no_piv_list ;
    with Done -> 
	piv := !piv + 1;
  done
  ;
  !no_piv_list

let insert_val vec elt pos = 
  let dim = Array.length vec in 
  
  let rec insert vec elt pos =
  if pos >= dim
  then () 
  else 
    begin
      let new_elt = vec.(pos) in
      vec.(pos) <- elt;
      insert vec new_elt (pos + 1)
    end
  in
  insert vec elt pos
 
let nullspace_computation mat = 
  
  let mat = copy_mat mat in
  let no_pivs = List.rev (rref mat) in 

  let dim2 = mat.rows in

  let num_pivs = mat.cols - List.length no_pivs in
  let vecs =
    
    List.fold_right
      (fun no_piv acc -> 
	  if dim2 = mat.cols
	  then
	    Array.map (F.mul mone)
	      (get_col_in_line mat no_piv) :: acc
	  else 
	    let new_vec = Array.make mat.cols F.zero
	    in
	    Array.iteri
	    (fun i f -> 
	      if i <= num_pivs
	      then new_vec.(i) <- F.mul mone f
	    )
	      (get_col_in_line mat no_piv)
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
	    insert_val vec F.zero not_piv2
	  else ()
	)
	no_pivs;
      insert_val vec F.one not_piv;

    )
    vecs 
    no_pivs;
  vecs
  
let nullspace m = 
  Mat_option.debug ~dkey:dkey_null
    "Nullspace computation";
  let t = Sys.time () in
  let res = nullspace_computation m in 
  let () = Mat_option.nullspace_timer := !Mat_option.nullspace_timer +. Sys.time() -. t in
  Mat_option.debug ~dkey:dkey_null
    "Nullspace done"; res

   
end
      

(** 2. Rational matrix implementation *)

module QMat = Make(Q)

module QPoly = struct include Poly.XMake(Q) end 

module Q_Set:Set.S with type elt = Q.t = Set.Make
  (Q)
module Z_Set:Set.S with type elt = Z.t = Set.Make
  (Z)


let lacaml_to_qmat lmat = 
  
  lmat 	 |> Lacaml_D.Mat.to_array 
	 |> Array.map 
	     (fun arr -> QMat.vec_from_array (Array.map (fun fl -> Q.of_float fl) arr)) 
	 |> QMat.from_array
  
let qmat_to_lacaml qmat = 
  
  let arr = QMat.to_array qmat in
  
  Array.map 
    (fun a -> Array.map
      (fun q -> (Z.to_float (Q.num q)) /. (Z.to_float (Q.den q))) a)
    arr
	 |> Lacaml_D.Mat.of_array

let lvec_to_qvec lvec = 
  lvec |> Lacaml_D.Vec.to_array
	 |> Array.map (fun fl -> Q.of_float fl)
	 |> QMat.vec_from_array

let qvec_to_lvec lvec = 
  let arr = QMat.vec_to_array lvec in
  Array.map (fun fl -> (Z.to_float (Q.num fl)) /. (Z.to_float (Q.den fl))) arr 
	 |> Lacaml_D.Vec.of_array

let char_poly (mat:QMat.t) = (* https://fr.wikipedia.org/wiki/Algorithme_de_Faddeev-Leverrier *)
  (* To find eigenvalues, we will compute the faddeev-leverrier iteration in order to find
     the extremities of the rational characteristic polynomial and then apply the rational root
     theorem.*)
  assert (Mat_option.Use_zarith.get ());
  let t = Sys.time () in
  let dim = (QMat.get_dim_col mat) in
  let identity = QMat.identity dim in

  let rec trace_mk index mk = 
    if index <= dim
    then 
      let mk_coef = Q.div (QMat.trace mk) (Q.of_int index) in
      
      let new_monom = (QPoly.monomial (Q.sub Q.zero mk_coef) [Poly.X,(dim-index)]) in
      
      let mkpo =  QMat.mul mat (QMat.sub mk (QMat.scal_mul identity mk_coef)) in

      QPoly.add new_monom (trace_mk (index + 1) mkpo)
    else (QPoly.monomial Q.one [Poly.X,dim])
    
  in
  let res = trace_mk 1 mat 
  in
  let () = Mat_option.char_poly_timer := !Mat_option.char_poly_timer +. Sys.time () -. t in res

let eigenvalues mat = 
  let t = Sys.time () in
  
  let deg_poly = (QMat.get_dim_col mat) in
  let integrate_poly p = (* returns the main coefficient of the polynomial after multiplying it
			    by a scalar such that the polynomial have only integer coefficients.*)
    let rec integ xn p = 
      if QPoly.deg_of_var xn Poly.X = deg_poly
      then Z.one (* by construction, its xn's coef is an integer *)
      else 
	let den_of_coef = 
	  Q.den (QPoly.coef p xn)
	in
	let xnpo = (QPoly.mono_mul xn (QPoly.mono_minimal [Poly.X,1]))
	in 
	let k = 
	  integ xnpo (QPoly.scal_mul (Q.of_bigint den_of_coef) p) in
	
	
	(Z.mul k den_of_coef)

    in
    integ QPoly.empty_monom p
  in
  let poly = char_poly mat in 
  
  let () = Mat_option.debug ~dkey:dkey_ev ~level:2
    "Char poly = %a" QPoly.pp_print poly

  in
  let k = integrate_poly poly in
  
    let div_by_x poly = (* returns (coef,n) with coef*xn with the littlest n such that xn | poly *)
    let rec __div_by_x monomial = 
      let coef = QPoly.coef poly monomial in
      Mat_option.debug ~dkey:dkey_ev ~level:4
	"Coef for div_by_x : %a" Q.pp_print coef;
      if Q.equal coef Q.zero
      then __div_by_x (QPoly.mono_mul monomial (QPoly.mono_minimal [Poly.X,1]))
      else (coef,(QPoly.deg_of_var monomial Poly.X))
    in
    __div_by_x QPoly.empty_monom
  in
  let (coef,power) = div_by_x poly
  in
  
  let affine_constant = Q.mul coef (Q.of_bigint k) 
  in
  assert (Q.den affine_constant = Z.one);
  let affine_constant = Q.num affine_constant in
  
  let () = Mat_option.debug ~dkey:dkey_ev ~level:2
    "Divisible by 0 %i times. Affine constant : %a"
    power Z.pp_print affine_constant in

  (*let max_number_of_roots = deg_poly - power
  in*)
  let all_divs (i:Z.t) : Z_Set.t = 
    let max_ev = Z.of_int (Mat_option.Ev_leq.get ()) in
    let rec __all_divs cpt i = 

      if  Z.gt (Z.shift_left cpt 1) i (* 2*p > i => p does not div i *)
	|| Z.geq cpt max_ev
      then 
	(Z_Set.singleton Z.one) 
	 |> Z_Set.add Z.minus_one 
      else if Z.erem i cpt = Z.zero 
      then 
	let divs = __all_divs cpt (Z.div i cpt) in
	Z_Set.fold
	  (fun elt -> Z_Set.add (Z.mul cpt elt) )
	  divs
	  divs
      else
	 __all_divs (Z.nextprime cpt) i
    in
    if Z.leq i Z.zero
    then 
      __all_divs (Z.of_int 2) (Z.mul Z.minus_one i)
      
      
    else 
      __all_divs (Z.of_int 2) i |> Z_Set.add i |> Z_Set.add (Z.mul i Z.minus_one)

  in
  
  let root_candidates =

    let divs_of_affine_coef = all_divs affine_constant in
    let divs_of_main_coef = all_divs k in
    
    Z_Set.iter
      (fun q -> 
	Mat_option.debug 
	  ~dkey:dkey_ev 
	  ~level:5
	  "Divisor of affine const : %a" Z.pp_print q) divs_of_affine_coef ; 
    Z_Set.iter
      (fun p -> 
	Mat_option.debug 
	  ~dkey:dkey_ev 
	  ~level:5
	  "Divisor of main const : %a" Z.pp_print p) divs_of_main_coef ; 
    
    
    Z_Set.fold
      (fun p acc -> 
	
	Z_Set.fold
	  (fun q acc2 -> 
	    Q_Set.add ((Q.(///)) p q) acc2)
	  divs_of_main_coef
	  acc
      )
      divs_of_affine_coef
      Q_Set.empty
  in
  let res = Q_Set.filter 
    (fun elt -> 
      let eval_poly = QPoly.eval poly Poly.X elt in
      Mat_option.debug ~dkey:dkey_ev ~level:4
	"Root candidate : %a. Returns %a" Q.pp_print elt QPoly.pp_print eval_poly;
      Q.equal (QPoly.coef eval_poly QPoly.empty_monom) Q.zero
    )
    root_candidates
  in
  let res = 
    if power = 0 then res 
    else Q_Set.add Q.zero res in


  let () = 
    Mat_option.ev_timer := !Mat_option.ev_timer +. Sys.time () -. t;
    Q_Set.iter 
      (fun ev -> 
	Mat_option.debug ~dkey:dkey_ev ~level:2 "Eigenvalue : %a" 
	  Q.pp_print ev) res
      
  in res

(** 3. Polynomial matrices implementation. 
    This is how we will deal with non deterministic loop *)

module N_id = State_builder.SharedCounter(struct let name = "nid_counter" end)

type n_var = 	
  {
    name:string;
    min:float;
    max:float
  }
    
module N_var =
  struct 
    include Datatype.Make_with_collections
    (struct
      type t = n_var
      let name = "P_string"
      let reprs = [{name = "n";min = -0.1; max = 0.1}]
      let compare n1 n2 = 
	let max = Pervasives.compare n1.max n2.max  in
	if max <> 0 then max 
	else 	
	  let min = Pervasives.compare n1.min n2.min  in
	  if min <> 0 then min
	  else String.compare n1.name n2.name
	
      let equal = (=)
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let structural_descr = Structural_descr.t_abstract
      let mem_project = Datatype.never_any_project
      let hash = Hashtbl.hash
      let rehash = Datatype.identity
      let pretty = Datatype.undefined
      let varname s = "str " ^ s.name
     end)
      
    let new_var min max = 
      { name = "n" ^ (string_of_int (N_id.next ()));
	min = min;
	max = max}
end
module P : Polynomial with type c = Float.t and type v = N_var.t = 
  Poly.Make(Float)(N_var)

module PMat : Matrix with type elt = P.t = Make (P)

let pmat_eval_to_zero m = 
  let dim = PMat.get_dim_col m in
  let res = Lacaml_D.Mat.create dim dim in
  ignore (
    PMat.mapi (* TODO : PMat.iteri would be better *)
      (fun i j poly -> 
	let affine_coef = P.coef poly P.empty_monom in
	let () = res.{i,j} <- affine_coef in 
	poly)
      m);
  
  res
    
let fvec_to_pvec f_vec : PMat.vec = 
  PMat.vec_from_array
    (Array.map
       P.const    
       (Lacaml_D.Vec.to_array f_vec)
    )
