open Poly
open Cil_types
open Pilat_matrix
open Cil_datatype
open Poly_affect 

exception Not_solvable


val block_to_poly_lists : Cil_types.block -> Poly_affect.body list
(** Returns a list of list of polynomial affectations. Each list correspond to a the 
    succession of affectations for each possible path in the loop.
    Raises Not_solvable if a statement of the loop is not solvable. *)

val add_monomial_modifications : 
  Poly_affect.body -> Poly_affect.monom_affect list * Poly_affect.F_poly.Monom.Set.t
(** Returns the list of monomial affectations needed to linearize the loop, and the
    set of all monomials used. *)

val loop_matrix : 
  int F_poly.Monom.Map.t -> Poly_affect.monom_affect list -> Lacaml_D.mat
(** Computes the lacaml matrix assoctated to the linearized loop. The first argument is the 
    base for the matrix : each monomial is associated to a line / column of the matrix. 
*)

val loop_qmat : int F_poly.Monom.Map.t -> (F_poly.Monom.t * F_poly.t) list -> QMat.t
(** Same, but outputs a zarith matrix. *)
