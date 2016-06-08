open Poly_affect

val prove_invariant : Lacaml_D.Mat.t -> int F_poly.Monom.Map.t -> Cil_types.predicate -> Property_status.emitted_status

val prove_annot : Lacaml_D.Mat.t -> int F_poly.Monom.Map.t -> Cil_types.code_annotation -> Property_status.emitted_status

