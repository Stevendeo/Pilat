type t = Lacaml_D.mat
type vec = Lacaml_D.vec

(** Computes the eigenvalues of a lacaml matrix.
    This function has several problems, as the lacaml library is
    not precise enough for big matrices. Therefore : 
    
    - Eigenvalues are not guaranteed to be correct
    - Complex eigenvalue are ignored 
*)
val eigen_val : t -> float list

(** Computes a base of the matrix in argument. *)
val nullspace_computation : t -> vec list
