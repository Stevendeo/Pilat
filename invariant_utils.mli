type vec = Lacaml_D.vec
type mat = Lacaml_D.mat

val invariant_computation : mat -> Pilat_matrix.QMat.vec list list

val intersection_invariants : (vec list) list -> (vec list) list -> (vec list) list

val invariant_computation_pilat : Pilat_matrix.QMat.t -> Pilat_matrix.QMat.vec list list

val intersection_invariants : (vec list) list -> (vec list) list -> (vec list) list

val intersection_invariants_pilat : Pilat_matrix.QMat.vec list list -> Pilat_matrix.QMat.vec list list -> Pilat_matrix.QMat.vec list list

val integrate_vec : Pilat_matrix.QMat.vec -> Pilat_matrix.QMat.vec
