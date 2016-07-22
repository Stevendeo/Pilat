(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

(** Lacaml matrices module *)

include Pilat_math.Matrix 
 with type elt = float
 and type vec = Lacaml_D.vec
 and type t = Lacaml_D.mat



(** Computes the eigenvalues of a lacaml matrix.
    This function has several problems, as the lacaml library is
    not precise enough for big matrices. Therefore : 
    
    - Eigenvalues are not guaranteed to be correct for big matrices
    - Complex eigenvalue are ignored 
*)
val eigen_val : t -> float list

