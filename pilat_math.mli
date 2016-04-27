(** This module conains the signature of high level modules used to reprensent 
    mathematical concepts, which are :

    - The Ring structure
    - The Polynomial structure. Polynomials can be with multiple variables.
    - The Matrix structure
*)

(** 1. The ring structure *)

module type Ring = sig
  type t
(* type of elements of the ring *)
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val equal : t -> t -> bool
  val pp_print : Format.formatter -> t -> unit
    
end

(** 2. The polynomial structure *)
module type Polynomial =
sig
  
  type c
    (** type of coefficients *)
  
  type v 
    (** type of variables *)
  
  module Monom : Datatype.S_with_collections
    
  type t
    (** type of polynoms *)

  val zero : t
  val one : t
    (* unit for the polynomial product.
       It is superfluous, since it is a special case of monomial;
       however this makes polynomials match the interface of rings *)
    
  val empty_monom : Monom.t
    
  val mono_poly : c -> Monom.t -> t
  val mono_minimal : (v * int) list -> Monom.t
  val monomial : c -> (v * int) list -> t
  val const : c -> t
    
  val to_var : Monom.t -> v list
    (** Same than before, but each variable appears only once *)
  val to_var_set : Monom.t -> v list
    
  val deg_monom : Monom.t -> int
  val deg_of_var : Monom.t -> v -> int
    
  val mono_mul : Monom.t -> Monom.t -> Monom.t
  val coef : t -> Monom.t -> c
    
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t (** Fails every time, but allows to match the RING signature. *)
  val scal_mul : c -> t -> t
  val pow : t -> int -> t
    
  val equal : t -> t -> bool
    
  val eval : t -> v -> c -> t
  val pp_print : Format.formatter -> t -> unit
    
  (** Composition of two polynoms *)
  val compo : t -> v -> t -> t
    
  val deg : t -> int
    
  val get_monomials : t -> Monom.Set.t
    
  val has_monomial : t -> Monom.t -> bool

end

(** 3. The matrix structure *)
module type Matrix = sig
    
  type elt
  type vec
  type t
  
  exception Dimension_error of int*int*int*int

  (** Matrix creation *)

  val zero : int -> int -> t
  val create_mat : int -> int -> (int -> int -> elt) -> t
  val copy_mat : t -> t
  val identity : int  -> t

  (** Getters and setters *)

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

  (** Iterators *)

  val map : (elt -> elt) -> t -> t
  val mapi : (int -> int -> elt -> elt) -> t -> t

  (** Operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val transpose : t -> t
  val scal_mul : t -> elt -> t
  val mul : t -> t -> t
  val pow : t -> int -> t
  val trace : t -> elt

  (** Nullspace computation *)
  val nullspace : t -> vec list

  (** Pretty printers *)
  val pp_print : Format.formatter -> t -> unit
  val pp_vec : Format.formatter -> vec -> unit


end  
