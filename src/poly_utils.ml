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

open Pilat_math
open Cil_datatype

(** 1. Variables used for polynomials *)

module N_id = State_builder.SharedCounter(struct let name = "nid_counter" end)

type n_var = 	
  {
    name:string;
    min:float;
    max:float
  }
   
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
      let pretty = Datatype.undefined
      let varname s = "str " ^ s.name
     end)
      
    let new_var min max = 
      { name = "n" ^ (string_of_int (N_id.next ()));
	min = min;
	max = max}
    
  end

(** 2. Polynomials *)

(** Polynomials for deterministic assignments *)
module QPoly = struct include Poly.Make(Qring)(Cil_datatype.Varinfo) end 

module F_poly : Polynomial with type c = Float.t 
			   and type v = Varinfo.t 
			   and type Var.Set.t = Varinfo.Set.t = 
				 Poly.Make(Float)(Varinfo)

(** Polynomial for non deterministic assignments *)

module N_poly : Polynomial with type c = Float.t and type v = N_var.t = 
  Poly.Make(Float)(N_var)

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

