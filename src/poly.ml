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

(* The functor that given a ring structure returns a polynomial structure.
   One must be careful to make the type of coefficients consistent with the
   type of the elements of the ring structure received as parameter *)

module Make (A : Ring) (V : Datatype.S_with_collections) : 
  Polynomial with type c = A.t
	     and type v = V.t
	     and type Var.Set.t = V.Set.t =
struct
  
    (* 1. Type declaration *)
  type c = A.t
    
  type v = V.t
    
  module R = A
  module Var = V
  module Monom: Datatype.S_with_collections with type t = (int V.Map.t) = struct 
  include (
    Datatype.Make_with_collections(
      struct
	include Datatype.Undefined
	type t = (int V.Map.t)
	let name = "Monom_"  ^ V.name
	let equal = V.Map.equal (fun (a:int) (b:int) -> a = b)   
	let hash = Hashtbl.hash (* For now, do not hash monomials *)
	let compare = V.Map.compare Pervasives.compare
	let varname (x:t) = 
	  V.Map.fold
	    (fun var pow acc -> (V.varname var) ^ (string_of_int pow) ^ acc)
	    x
	    ""
	let reprs = [V.Map.empty]
	  
	let copy = Datatype.identity
	let rehash = Datatype.identity
	let mem_project = Datatype.never_any_project
      end)
  )
  let pretty fmt (m:t) = 
    V.Map.iter
      (fun var pow ->  
	match pow with
	| 1 -> Format.fprintf fmt "%a" V.pretty var
	| _ -> 
	  Format.fprintf fmt "(%a^%i)" V.pretty var pow)
      m
  end
    
      
  type m = (int V.Map.t) (* x2*y -> map from x to 2 and y to 1 *)
   
  (* for simplicity *)
  module P = Monom.Map
  type t = c P.t 
    

   

  let (zero:t) = P.empty
    
  let empty_monom = V.Map.empty

  let equal (p1:t) (p2:t) : bool = P.equal A.equal p1 p2

    
      
    (* monomial creation *)
  let mono_poly (a:A.t) (m:Monom.t) : t =
    P.singleton m a

  let mono_minimal (l:(v * int) list) : Monom.t = 
    List.fold_left
      (fun acc (v,i) -> 
	if i = 0 then acc else
	V.Map.add v i acc
      )
      V.Map.empty l

  let monomial (a:A.t) (l:(v * int) list) : t = 
    mono_poly a (mono_minimal l)
       
  let var_to_monom (v:v) = mono_minimal [v,1]

  let to_var (m:Monom.t) : v list = 
    let rec add_i_var v i acc = 
      match i with 
	0 -> acc
      | _ -> add_i_var v (i-1) (v :: acc)
    in
    V.Map.fold
      add_i_var
      m
      []

  let to_var_set (m:Monom.t) : v list = 
    V.Map.fold
      (fun v _ acc -> v :: acc)
      m
      []

  let deg_of_var (m:Monom.t) (v:v) : int = 
    try V.Map.find v m with Not_found -> 0 

  let const (c:A.t) = mono_poly c V.Map.empty

    (* a pacticular case *)
    let (one:t) = const A.one


    let simple_op (op: A.t -> A.t -> A.t) : t -> t -> t = 
      P.merge
	(fun _ m1 m2 -> 
	  match m1,m2 with
	    None, None -> None
	  | None, Some m -> Some (op A.zero m)
	  | Some m, None -> Some m
	  | Some c1, Some c2 -> Some (op c1 c2)
	) 
 

    let add : t -> t -> t =
      simple_op A.add
      
    let sub : t -> t -> t = simple_op A.sub

    let mono_mul : m -> m -> m = 
      V.Map.merge
	(fun _ v1 v2 -> 
	  match v1,v2 with
	    None,None -> None
	  | Some k, None 
	  | None, Some k -> Some k
	  | Some k1, Some k2 -> Some (k1+k2)
	)
        
    let coef (p:t) (m:Monom.t) : c = 
      try P.find m p with Not_found -> A.zero

    let fois (k:A.t) (m:m) (p:t) : t =
      
      P.fold
	(fun monom coeff acc -> 
	  let new_monom = mono_mul monom m
	  in
	  add acc (mono_poly (A.mul coeff k) new_monom)	  
	)
	p
	zero

    let scal_mul (c:c) = 
      P.map (A.mul c)
      

    let mul (p1:t) (p2:t) : t =     
      P.fold
	(fun monom coeff acc -> 
	  let new_monom = (fois coeff monom p2)
	  in
	  add acc new_monom
	)
	p1
	zero
    
    let div _ _ = assert false

    let rec pow_ring (c:c) (n:int) : c = 
      match n with
	0 -> A.one
      | 1 -> c
      | _ -> 
	if n mod 2 = 0
	then pow_ring (A.mul c c) (n/2)
	else A.mul c (pow_ring (A.mul c c) ((n-1)/2))
      

    let eval (p:t) (x:v) (xval:c) : t =
      Monom.Map.fold
	(fun (m:Monom.t) (coef:c) acc -> 
	  if V.Map.mem x m
	  then 
	    let pow = 
	      V.Map.find x m
	    in
	    let mono_poly = 
	      mono_poly 
		(A.mul coef (pow_ring xval pow)) 
		(V.Map.remove x m) in
	    add acc mono_poly
	  else add acc (mono_poly coef m)
	)
	p
	zero

    let print_monom = Monom.pretty
	

    let pp_print fmt (p:t) =
      if p = zero then Format.fprintf fmt "0" else

        P.iter
	  (fun monom coef -> 
	    Format.fprintf fmt " + %a" A.pp_print coef;
	    (print_monom fmt monom)
	  )
          p
	  

    (* Computation of (p1 o p2) *)

    let rec pow (p:t) = function
      | 0 -> one
      | 1 -> p
      | k -> 
      
	if k mod 2 = 0
	then 
	  let p_pow_half = (pow p (k/2))
	  in
	  mul p_pow_half p_pow_half
	else
	  let p_pow_half = (pow p ((k-1)/2))
	  in
	  mul p (mul p_pow_half p_pow_half)
      

    (* Replaces the occurence of v in m by p, and multiplies the resulting polynom by k *)
    let compo_monom (k:A.t) (monom:m) (v:V.t) (p:t) : t = 
      if V.Map.mem v monom
      then 
	let assoc_pow = V.Map.find v monom
	in
	let poly_pow = pow p assoc_pow
	in 
	let old_poly = mono_poly k (V.Map.remove v monom) in 
	
	mul poly_pow old_poly
	
      else mono_poly k monom
      
    let compo (p1:t) (v:V.t) (p2:t) : t = 
      P.fold
	(fun monom coeff acc -> 
	  add acc (compo_monom coeff monom v p2))
	p1
	zero

    let deg_monom (m:Monom.t) : int = 
       V.Map.fold
	 (fun _ coef acc -> acc + coef)
	 m
	 0
  

    let deg (p:t) : int = 
      P.fold
	(fun var_map _ deg -> 
	  max deg (deg_monom var_map)	   
	)
	p
	(-1)

   
    let get_monomials (p:t) : Monom.Set.t = 
      P.fold
	(fun m _ acc -> Monom.Set.add m acc) 
	p
	Monom.Set.empty

    let has_monomial (p:t) (m:Monom.t)  : bool = 
      P.mem m p

    let float_to_t (f:float) = const (A.float_to_t f)

    let approx (p:t) =
      Monom.Map.fold 
	(fun monom coef acc -> 
	  add (mono_poly (A.approx coef) monom) acc
	)
	p
	zero

    let leq = assert false
  end


(*
let i = F.monomial 2. [("a",1);("b",2)];;
let j = F.monomial 1. [("a",1);("b",2);("c",3)];;

F.print Format.std_formatter (F.add (F.monomial 2. []) j);;
F.print Format.std_formatter (F.compo (F.add i j) "b" (F.add (F.const 2.) j));;
*)

type var = | X

module XMake (A:Ring) : (Polynomial with type c = A.t and type v = var) 
 = 

  Make 
    (A) 
    (Datatype.Make_with_collections 
       (struct  
	 type t = var
	 let compare _ _ = 0
	 let copy _ = X
	 let name = "X" 
	 let hash = Hashtbl.hash
	 let rehash s = s
	 let structural_descr = Structural_descr.t_abstract
	 let reprs = [X]
	 let equal = (=)
	 let internal_pretty_code = Datatype.undefined
	 let pretty fmt _ = Format.fprintf fmt "X"
	 let varname _ = "X"
	 let mem_project = Datatype.never_any_project
	end
       )
    );;
  
