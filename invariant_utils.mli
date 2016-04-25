type vec = Pilat_matrix.QMat.vec 
type mat = Lacaml_D.mat 

(** Returns the rational eigenspaces union of the floating matrix 
    as a list of bases. *)
val invariant_computation : mat -> vec list list

(** Intersects two union of vectorial spaces. *)
val intersection_invariants :  vec list list -> vec list list -> vec list list

(** After the integration, there is no fraction left on the vector expression. *)
val integrate_vec : vec -> vec
