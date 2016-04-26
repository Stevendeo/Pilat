open Pilat_math

(** Polynomials with multiple variables. *)    
module Make : functor (A : Ring) (V : Datatype.S_with_collections) -> 
  (Polynomial with type c = A.t and type v = V.t) 
    
type var = | X

(** Polynomial with a unique variable, X. *)
module XMake : functor (A : Ring) -> (Polynomial with type c = A.t and type v = var) 
			
