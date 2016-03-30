(** the ring structure *)
module type RING =
  sig
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val equal : t -> t -> bool
    val pp_print : Format.formatter -> t -> unit
  end
      

(** the polynom structure *)

(* la structure d'anneau sur des valeurs de type t *)
module type POLYNOMIAL =
  sig

    type c
        (* type of coefficients *)

    type v 
        (* type of variables *)

    module Monom:Datatype.S_with_collections

    type t
        (* type of polynoms *)


    val zero : t
    val one : t
        (* unit for the polynomial product.
           It is superfluous, since it is a special case of monomial;
           however this makes polynomials match the interface of rings *)

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
    val pow : t -> int -> t

    val equal : t -> t -> bool

    val pp_print : Format.formatter -> t -> unit

    (** Composition of two polynoms *)
    val compo : t -> v -> t -> t

    val deg : t -> int

    val get_monomials : t -> Monom.Set.t
    val has_monomial : t -> Monom.t -> bool

  end
      
module Make : functor (A : RING) (V : Datatype.S_with_collections) -> 
  (POLYNOMIAL with type c = A.t and type v = V.t) 
    
