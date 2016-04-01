type vec = Pilat_matrix.QMat.vec 
type mat = Lacaml_D.mat 

val invariant_computation : mat -> Pilat_matrix.QMat.vec list list

val intersection_invariants :  vec list list -> vec list list -> vec list list

val integrate_vec : vec -> vec
