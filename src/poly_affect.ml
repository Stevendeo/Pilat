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

exception Incomplete_base


module type S = sig 

  type coef (** Coefficient of the polynomial *)
  type var (** Variables used by the polynomial *)

  type mat (** Matrix in which the affectation will be translated *)

  module P : 
    (sig 
      include Polynomial      

      (** Takes a monomial and its affectation, returns a matrix and its base. 
	  If a base is provided it will complete it and use it for the matrix, else it 
	  will create a new base from the affectation.
	  Raises Incomplete_base if unconsidered variables are necessary for the matrix.
      *)
      val to_mat : ?base:int Monom.Map.t -> Monom.t -> t -> int Monom.Map.t * mat
     end)

  type t = Affect of var * P.t
  and body = t list

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
  type monom_affect = P.Monom.t * P.t

end

module Make (P:Polynomial)(M:Matrix with type elt = P.c) : 
  S with type coef = P.c 
      and type var = P.v
      and type mat = M.t = 
struct 
  type coef = P.c
  type var = P.v
  type mat = M.t
  module P = 
  struct 
    include P
    let to_mat ?(base = Monom.Map.empty) (monom_var:Monom.t) (p:t) : int Monom.Map.t * mat = 
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
      let size_base = (Monom.Map.cardinal base_monom) in
      let mat = M.zero size_base size_base in
              
      let row = Monom.Map.find monom_var base_monom in 
      
      let () = 
	Monom.Set.iter
	  (fun m -> 
	      let col_monom = 
		try Monom.Map.find m base_monom 
		with Not_found -> raise Incomplete_base
	      in
	      let coef = coef p m in
	      M.set_coef row col_monom mat coef
	  )
	  (get_monomials p)
	  
      in
      base_monom,mat 
  end
    
  type t = Affect of var * P.t
  and body = t list

  (** A monomial affectation is equivalent to considering a monomial is a variable modified
    by the affectation. *)
  type monom_affect = P.Monom.t * P.t

end
