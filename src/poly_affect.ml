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
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3                   *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

open Pilat_math

exception Incomplete_base

type var =  Cil_datatype.Varinfo.t

module Ring = 
struct 
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let mul = ( *. )
  let sub = (-.)
  let div = (/.)
  let equal = (=)
  let pp_print fmt i = 
    Format.fprintf fmt "%.3f" i 
end

module type Extended_Poly = 
sig 
  include Polynomial
  val to_lacal_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * Lacaml_D.mat
end

module F_poly: Extended_Poly with type c = Ring.t and type v = var = 
  
struct 
  include Poly.Make(Ring)(Cil_datatype.Varinfo)
  let to_lacal_mat ?(base = Monom.Map.empty) (monom_var:Monom.t) (p:t) : int Monom.Map.t * Lacaml_D.mat = 
      let base_monom = 
	if Monom.Map.is_empty base
	then 
	  let poly_base = 
	    Monom.Set.add monom_var (get_monomials p) in 
	  let i = ref 0 in 
	  Monom.Set.fold
	    (fun m map ->
	      i := !i + 1;
	      Monom.Map.add m !i map
	    )
	    poly_base
	    Monom.Map.empty
	else base
	    
      in
      
      let mat = Lacaml_D.Mat.identity (Monom.Map.cardinal base_monom) in
		
      let ext_poly = 
	if has_monomial p monom_var 
	then p
	else (add (mono_poly Ring.zero monom_var) p)
      (* p + 0*v, so the next iteration sets to zero the unit of the identity *)
	    
      in
        
      let row = Monom.Map.find monom_var base_monom in 
      
      let () = 
	Monom.Set.iter
	  (fun m -> 
	      let col_monom = 
		try Monom.Map.find m base_monom 
		with Not_found -> raise Incomplete_base
	      in
	      let coef = coef p m in
	      mat.{row,col_monom}<-coef
	  )
	  (get_monomials ext_poly)
      in
      base_monom,mat 

end

type t = 
  
  Affect of F_poly.v * F_poly.t
| Loop of body

and body = t list

type monom_affect = F_poly.Monom.t * F_poly.t

type if_cond = bool * Cil_types.exp

