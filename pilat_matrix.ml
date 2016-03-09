
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

  (** 3. Iterators *)

  val map : (elt -> elt) -> t -> t
  val mapi : (int -> int -> elt -> elt) -> t -> t

  (** 4. Operations *)
  val add : t -> t -> t
  val transpose : t -> t
  val scal_mul : t -> elt -> t
  val mul : t -> t -> t
  val pow : t -> int -> t

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

  let transpose mat = 
    let d1 = Array.length mat.m in
    if d1 = 0 then mat
    else 
      let d2 = Array.length mat.m.(0) in 
      create_mat d1 d2 (fun i j -> mat.m.(j).(i))

  let scal_mul m k = map (F.mul k) m
    

  let mul m1 m2 = 
    if m1.cols <> m2.rows
    then error m1 m2
    else
    let scal i j = 
      Format.print_int i;
      Format.print_int j;
      let _,res = 
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
    (Format.fprintf fmt "%a ," F.pp_print)
    v;
  Format.fprintf fmt ")\n"

let pp_print fmt mat = 
   Format.fprintf fmt "(";
     Array.iter
     (pp_vec fmt)
     mat.m;
  Format.fprintf fmt ")\n"
 
end
      

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
