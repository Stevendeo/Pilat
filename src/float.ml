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


type t = float
let zero = 0.
let one = 1.
let add = (+.)
let mul = ( *. )
let sub = (-.)
let div = (/.)
let equal f1 f2 = abs_float (f1 -. f2) < 1e-5 
let leq = (<=)
let geq = (>=)
let lt = (<)
let gt = (>)
let compare = Pervasives.compare
let pp_print fmt i = 
  Format.fprintf fmt "%.3f" i 
let to_str = string_of_float

let t_to_float f = f
let float_to_t f = f
let approx coef = 
  if coef < 0. then ceil coef
  else floor coef 

let den _ = assert false

let int_to_t = float_of_int
let t_to_int = int_of_float 

let non_det_repr f1 f2 = 
  if f1 = f2 then f1
  else 
      Mat_option.abort 
	"Deterministic float library used for non deterministic assignments"

let deter f = f

let to_nvars _ = []
