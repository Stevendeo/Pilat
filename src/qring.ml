(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

include (Q: Pilat_math.Ring_base with type t = Q.t)

let float_to_t = Q.of_float
let approx _ = assert false
let den f = f |> Q.den |> Q.of_bigint
let t_to_int = Q.to_int
let int_to_t = Q.of_int
let t_to_float q =
  ((q |> Q.num |> Z.to_float)
  /.
    (q |> Q.den |> Z.to_float))


let non_det_repr f1 f2 =
  if f1 = f2 then float_to_t f1
  else
      Mat_option.abort
	"Deterministic rational library used for non deterministic assignments."

let deter = t_to_float

let max = t_to_float
let min = t_to_float

let to_str = Q.to_string
let of_str f = float_to_t (float_of_string f)
let to_nvars _ = []
