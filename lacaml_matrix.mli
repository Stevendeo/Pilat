type t = Lacaml_D.mat
type vec = Lacaml_D.vec

val copy_mat : t -> t

val eigen_val : t -> float list

val nullspace_computation : t -> vec list
