open Cil_types
open Cil_datatype 

val add_loop_annots_zarith : 
  kernel_function 
  -> stmt 
  -> int Poly_affect.F_poly.Monom.Map.t 
  -> Pilat_matrix.QMat.vec list list 
  -> unit

