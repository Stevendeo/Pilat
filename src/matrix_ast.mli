open Pilat_matrix
open Poly_affect 
open Cil_datatype
exception Not_solvable


val exp_to_poly : Cil_types.exp -> F_poly.t

val block_to_poly_lists : Varinfo.Set.t -> Cil_types.block -> Poly_affect.body list
(** Returns a list of list of polynomial affectations. Each list correspond to a the 
    succession of affectations for each possible path in the loop, while omitting 
    variable absent of the set in argument
    Raises Not_solvable if a statement of the loop is not solvable. *)

val add_monomial_modifications : 
  Poly_affect.body -> Poly_affect.lin_body list * Poly_affect.F_poly.Monom.Set.t
(** Returns the list of monomial affectations needed to linearize the loop, and the
    set of all monomials used. *)

val loop_matrix : 
  int F_poly.Monom.Map.t -> Poly_affect.monom_affect list -> Lacaml_D.mat
(** Computes the lacaml matrix assoctated to the linearized loop. The first argument is the 
    base for the matrix : each monomial is associated to a line / column of the matrix. 
*)

val loop_qmat : int F_poly.Monom.Map.t -> Poly_affect.monom_affect list -> QMat.t
(** Same, but outputs a zarith matrix. *)
