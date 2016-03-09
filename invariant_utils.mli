type vec = Lacaml_D.vec
type mat = Lacaml_D.mat

val invariant_computation : mat -> (vec list) list

val intersection_invariants : (vec list) list -> (vec list) list -> (vec list) list
