open Poly
open Cil_types
open Pilat_matrix
open Cil_datatype

exception Not_solvable

module Ring : RING with type t = float

module F_poly : POLYNOMIAL with type c = Ring.t and type v = Cil_datatype.Varinfo.t

val block_to_poly_lists : Cil_types.block -> ((varinfo * F_poly.t) list) list

val add_monomial_modifications : 
  (varinfo * F_poly.t) list 
  -> ((F_poly.Monom.t * F_poly.t) list) * F_poly.Monom.Set.t

val loop_matrix : int F_poly.Monom.Map.t -> (F_poly.Monom.t * F_poly.t) list -> Lacaml_D.mat

val loop_qmat : int F_poly.Monom.Map.t -> (F_poly.Monom.t * F_poly.t) list -> QMat.t

