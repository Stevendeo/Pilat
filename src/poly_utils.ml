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

open Pilat_math

(** 1. Variables used for polynomials *)

module N_id = State_builder.SharedCounter(struct let name = "pilat_non_det_var_counter" end)

type n_var = Pilat_math.n_var


module Varinfo =
  struct
    include Cil_datatype.Varinfo
    let max _ = assert false
    let min _ = assert false
    let to_nvars _ = []
  end

module N_var =
  struct
    include Datatype.Make_with_collections
    (struct
      type t = n_var
      let name = "P_string"
      let reprs = [{name = "n";min = -0.1; max = 0.1}]
      let compare n1 n2 =
	let max = Pervasives.compare n1.max n2.max  in
	if max <> 0 then max
	else
	  let min = Pervasives.compare n1.min n2.min  in
	  if min <> 0 then min
	  else String.compare n1.name n2.name

      let equal = (=)
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let structural_descr = Structural_descr.t_abstract
      let mem_project = Datatype.never_any_project
      let hash = Hashtbl.hash
      let rehash = Datatype.identity
      let pretty fmt var = Format.fprintf fmt "%s" var.name
      let varname s = "str " ^ s.name
     end)

    let non_det_repr min max =
      { name = "n" ^ (string_of_int (N_id.next ()));
	min = min;
	max = max}
    let max t = t.max
    let min t = t.min
    let to_nvars t = [t]

  end

(** 2. Polynomials *)

(** Polynomials for deterministic assignments *)
module QPoly = struct include Poly.Make(Qring)(Varinfo) end

module F_poly : Polynomial with type c = Float.t
			   and type v = Varinfo.t
			   and type Var.Set.t = Varinfo.Set.t =
				 Poly.Make(Float)(Varinfo)

(** Polynomial for non deterministic assignments *)

module N_poly : Polynomial with type c = Float.t and type v = N_var.t =
struct
  include Poly.Make(Float)(N_var)
  let non_det_repr f1 f2 = mono_poly 1. (var_to_monom (N_var.non_det_repr f1 f2))
end
module NF_poly : Polynomial with type c = N_poly.t
			    and type v = Varinfo.t
			    and type Var.Set.t = Varinfo.Set.t=
				  Poly.Make
				    (N_poly)
				    (Varinfo)

module NQ_poly : Polynomial with type c = Q.t and type v = N_var.t =
  Poly.Make(Qring)(N_var)

module NQF_poly : Polynomial with type c = NQ_poly.t
			    and type v = Varinfo.t
			    and type Var.Set.t = Varinfo.Set.t=
				  Poly.Make(NQ_poly)(Varinfo)
