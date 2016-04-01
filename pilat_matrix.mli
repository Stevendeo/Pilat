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


module Make :
  functor (F : Field) -> M with type elt = F.t
  
module QMat : M with type elt = Q.t
  
module QPoly : Poly.POLYNOMIAL with type c = Q.t
module Q_Set:Set.S with type elt = Q.t

val char_poly : QMat.t -> QPoly.t

val eigenvalues : QMat.t -> Q_Set.t

val lvec_to_qvec : Lacaml_D.vec -> QMat.vec
val qvec_to_lvec : QMat.vec -> Lacaml_D.vec 

val lacaml_to_qmat : Lacaml_D.Mat.t -> QMat.t
val qmat_to_lacaml : QMat.t -> Lacaml_D.Mat.t
