open Pilat_math

(** 1. Matrices *)

(** Given a ring, creates a matrix module.
    Iterations are made line by line.
*)
module Make :
  functor (F : Ring) -> Matrix with type elt = F.t
  
(** Matrix with rational coefficients *)
module QMat : Matrix with type elt = Q.t
  
(** Polynomial with rational coefficients *)
module QPoly : Polynomial with type c = Q.t

(** Set of rational numbers *)
module Q_Set:Set.S with type elt = Q.t

(** Matrix utilities *)

(** Characteristic polynomial of a rational matrix *)
val char_poly : QMat.t -> QPoly.t

(** Rational eigenvalues of a matrix. Computation of the 
    rational roots of the characteristic polynomial.
      ^
     /|\
    /_o_\ 
    
    If the characteristic polynomial is to big, will only test
    a subset of all possible eigenvalues.
*)
val eigenvalues : QMat.t -> Q_Set.t

(** Float <-> Rational translaters *)
val lvec_to_qvec : Lacaml_D.vec -> QMat.vec
val qvec_to_lvec : QMat.vec -> Lacaml_D.vec 

val lacaml_to_qmat : Lacaml_D.Mat.t -> QMat.t
val qmat_to_lacaml : QMat.t -> Lacaml_D.Mat.t
