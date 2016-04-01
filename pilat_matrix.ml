let dkey_ev = Mat_option.register_category "pilat_matrix:eigenvalue"

module type Field = 
  sig 
    include Poly.RING
      
    val div : t -> t -> t
  end 

module type M = sig
    
  type elt
  type vec
  type t
  
  exception Dimension_error of int*int*int*int

  (** 1. Matrix creation *)

  val zero : int -> int -> t
  val create_mat : int -> int -> (int -> int -> elt) -> t
  val copy_mat : t -> t
  val identity : int  -> t

  (** 2. Getters and setters *)

  val get_row : t -> int -> t
  val get_col : t -> int -> t
  val get_col_in_line : t -> int -> vec
  val get_dim_col : t -> int
  val get_dim_row : t -> int

  val vec_to_array : vec -> elt array
  val vec_from_array : elt array -> vec

  val to_array : t -> elt array array
  val from_array : vec array -> t

  val set_coef : int -> int -> t -> elt -> unit
  val get_coef : int -> int -> t -> elt
	
  (** 3. Iterators *)

  val map : (elt -> elt) -> t -> t
  val mapi : (int -> int -> elt -> elt) -> t -> t

  (** 4. Operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val transpose : t -> t
  val scal_mul : t -> elt -> t
  val mul : t -> t -> t
  val pow : t -> int -> t
  val trace : t -> elt

  (** 5. Nullspace computation *)
  (* Changes the input !! *)
  val nullspace : t -> vec list

  (** 6. Pretty printers *)
  val pp_print : Format.formatter -> t -> unit
  val pp_vec : Format.formatter -> vec -> unit
end  

module Make (F:Field) : M with type elt = F.t = 
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

  let transpose mat = 
    let d1 = Array.length mat.m in
    if d1 = 0 then mat
    else 
      let d2 = Array.length mat.m.(0) in 
      create_mat d2 d1 (fun i j -> mat.m.(j).(i))

  let scal_mul m k = map (F.mul k) m
    

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

(** 3. Nullspace computation *)

let revert_rows mat a b = 
  (*assert dim1 mat = dim2 mat *)
  let tmp = mat.m.(a) in
  mat.m.(a) <- mat.m.(b);
  mat.m.(b) <- tmp

let mult_row_plus_row mat a b k = (* sets the row a to a + k*b *)
  (* assert dim1 mat = dim2 mat *)
   mat.m.(a) <-
  Array.mapi
    (fun i elt -> F.add elt (F.mul k mat.m.(b).(i)))
    mat.m.(a)

let mult_row mat a k = (* sets the row a to k*a *)
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
 
let nullspace mat = 
  
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
   
end
      
(*
module Ring = 
struct 
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let mul = ( *. )
  let sub = (-.)
  let div = (/.)
  let equal = (=)
  let pp_print fmt i = 
    Format.fprintf fmt "%.3f" i 
end
*)
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
  let integrate_poly p = (* gets all divisors of the main elt of k.p, with k.p a polynomial
			    with integer coeficients and also returns k. *)
    let rec integ xn p = 
      if QPoly.deg_of_var xn Poly.X = deg_poly
      then Z.one,Z_Set.singleton Z.one (* by construction, its xn's coef is an integer *)
      else 
	let den_of_coef = 
	  Q.den (QPoly.coef p xn)
	in
	let xnpo = (QPoly.mono_mul xn (QPoly.mono_minimal [Poly.X,1]))
	in 
	let k,integ_set = 
	  integ xnpo (QPoly.scal_mul (Q.of_bigint den_of_coef) p) in
	
	
	(Z.mul k den_of_coef),

	(Z_Set.fold 
	   (fun elt acc -> 
	     Z_Set.add 
	       (Z.mul elt den_of_coef)
	       acc)
	   
	   integ_set
	   integ_set)
    in
    integ QPoly.empty_monom p
  in
  let poly = char_poly mat in 
  let k,divisors = integrate_poly poly in
  
  let () = 
    Mat_option.debug ~dkey:dkey_ev ~level:2
      "Polynomial : %a\n First parameter of the integrated poly : %a"
      QPoly.pp_print poly
      Z.pp_print k
  in

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

  let max_number_of_roots = deg_poly - power
  in
  let all_divs (i:Z.t) : Z_Set.t = 
    (* Set of all positive divisors, the boolean is set to true if i is negative *) 
    let rec __all_divs cpt i = 
      if  Z.geq (Z.shift_left cpt 1) i 
      then Z_Set.add Z.minus_one (Z_Set.singleton Z.one)
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
      __all_divs (Z.of_int 2) i

  in
  
  let divs_of_main_coef = all_divs affine_constant in
  
  let root_candidates = 
    Z_Set.fold
      (fun p acc -> 
	Z_Set.fold
	  (fun q acc2 -> 
	    Q_Set.add ((Q.(///)) p q) acc2)
	  divisors
	  acc
      )
      divs_of_main_coef
      Q_Set.empty
  in
  let res = Q_Set.filter 
    (fun elt -> 
      let eval_poly = QPoly.eval poly Poly.X elt in
      Mat_option.debug ~dkey:dkey_ev ~level:2
	"Root candidate : %a. Returns %a" Q.pp_print elt QPoly.pp_print eval_poly;
      
      
      Q.equal (QPoly.coef eval_poly QPoly.empty_monom) Q.zero
    )
    root_candidates
  in
  let () = Mat_option.ev_timer := !Mat_option.ev_timer +. Sys.time () -. t in res
