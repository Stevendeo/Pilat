open Cil_datatype
open Pilat_math

type var =  Varinfo.t

module Ring : Ring with type t = float
(** Ring of floats *)

module type Extended_Poly = 
sig 
  include Polynomial

  (** Takes a monomial and its affectation, returns a matrix and its base. 
      If a base is provided it will complete it and use it for the matrix, else it 
      will create a new base from the affectation.
  *)
  val to_lacal_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * Lacaml_D.mat

end


(** Multivariables float polynomials  *)
module F_poly : 
  Extended_Poly with type c = Ring.t and type v = var

(** Polynomial affectation *)
type t = 
  
  Affect of F_poly.v * F_poly.t
| Loop of body

and body = t list

(** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
type monom_affect = F_poly.Monom.t * F_poly.t


(* vvv -- Undefined -- vvv *)
type if_cond = bool * Cil_types.exp



