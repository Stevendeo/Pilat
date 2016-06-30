open Cil_datatype
open Pilat_math

exception Incomplete_base

type var =  Varinfo.t

module Ring : Ring with type t = float
(** Ring of floats *)

module type Extended_Poly = 
sig 
  include Polynomial

  (** Takes a monomial and its affectation, returns a matrix and its base. 
      If a base is provided it will complete it and use it for the matrix, else it 
      will create a new base from the affectation.
      Raises Incomplete_base if unconsidered variables are necessary for the matrix.
  *)
  val to_lacal_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * Lacaml_D.mat

end


(** Multivariables float polynomials  *)
module F_poly : 
  Extended_Poly with type c = Ring.t and type v = var

(** Polynomial affectation *)
type t = 
  
  Affect of F_poly.v * F_poly.t
| Loop of body list

and body = t list

(** A monomial affectation is equivalent to consider that a monomial is a variable modified
    by the affectation. *)
type monom_affect = 
  
  LinAffect of F_poly.Monom.t * F_poly.t
| LinLoop of lin_body list

and lin_body = monom_affect list




