open Pilat_math

(** Given a ring, creates a matrix module. *)
module Make :
  functor (F : Ring) -> Matrix with type elt = F.t
  
module QMat : Matrix with type elt = Q.t
  
module QPoly : Polynomial with type c = Q.t
module Q_Set:Set.S with type elt = Q.t

val char_poly : QMat.t -> QPoly.t

val eigenvalues : QMat.t -> Q_Set.t

val lvec_to_qvec : Lacaml_D.vec -> QMat.vec
val qvec_to_lvec : QMat.vec -> Lacaml_D.vec 

val lacaml_to_qmat : Lacaml_D.Mat.t -> QMat.t
val qmat_to_lacaml : QMat.t -> Lacaml_D.Mat.t
