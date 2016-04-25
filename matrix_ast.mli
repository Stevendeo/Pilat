open Poly
open Cil_types
open Pilat_matrix
open Cil_datatype
open Poly_affect 

exception Not_solvable

type poly_stmt = Poly_affect.t

val block_to_poly_lists : Cil_types.block -> poly_stmt list list

val add_monomial_modifications : 
  Poly_affect.t list 
  -> ((Poly_affect.F_poly.Monom.t * Poly_affect.F_poly.t) list) * Poly_affect.F_poly.Monom.Set.t

val loop_matrix : 
  int F_poly.Monom.Map.t -> (F_poly.Monom.t * F_poly.t) list -> Lacaml_D.mat

val loop_qmat : int F_poly.Monom.Map.t -> (F_poly.Monom.t * F_poly.t) list -> QMat.t

