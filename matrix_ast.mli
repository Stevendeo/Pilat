open Poly
open Cil_types
open Pilat_matrix
open Cil_datatype

exception Not_solvable

module Ring : RING

module F_poly : POLYNOMIAL with type c = Ring.t and type v = Cil_datatype.Varinfo.t

val block_to_poly_lists : Cil_types.block -> ((varinfo * F_poly.t) list) list

val add_monomial_modifications : (varinfo * F_poly.t) list -> ((F_poly.Monom.t * F_poly.t) list) * F_poly.Monom.Set.t

val loop_matrix : Varinfo.Set.t -> (varinfo * F_poly.t) list -> int F_poly.Monom.Map.t * Lacaml_D.mat

val loop_qmat : Varinfo.Set.t -> (varinfo * F_poly.t) list -> int F_poly.Monom.Map.t * QMat.t

