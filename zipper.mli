(***********************************************************************)
(*                                                                     *)
(*  OCaml library from the book ``Apprendre à programmer avec OCaml''  *)
(*                                                                     *)
(*  Sylvain Conchon and Jean-Christophe Filliâtre                      *)
(*  Université Paris Sud                                               *)
(*                                                                     *)
(*  Copyright 2014 Université Paris Sud.  All rights reserved. This    *)
(*  file is distributed under the terms of the GNU Library General     *)
(*  Public License, with the same special exception on linking as the  *)
(*  OCaml library. See http://caml.inria.fr/ocaml/license.fr.html      *)
(*                                                                     *)
(***********************************************************************)

type 'a zipper = { left: 'a list; right: 'a list; }
  
let empty = {left = []; right = []}

let of_list l =
  { left = []; right = l }
    
let move_right z = match z.right with
  | [] -> invalid_arg "move_right"
  | x :: r -> { left = x :: z.left; right = r }
      
let move_left z = match z.left with
  | [] -> invalid_arg "move_left"
  | x :: l -> { left = l; right = x :: z.right }
      
let to_list z =
  List.rev_append z.left z.right
      
let insert z x =
  { z with left = x :: z.left }
    
let delete_left z =  match z.left with
  | [] -> invalid_arg "delete_left"
  | _ :: l -> { z with left = l }
    
let delete_right z =  match z.right with
  | [] -> invalid_arg "delete_right"
  | _ :: r -> { z with right = r }

let get_left z = 
  try List.hd z.left with Failure "hd" -> raise (Failure "get_left")

let get_right z = 
  try List.hd z.right with Failure "hd" -> raise (Failure "get_right")

let rec reset_left z = try reset_left (move_left z) with _ -> z

let rec reset_right z =  try reset_right (move_right z) with _ -> z

let iter f z = List.fold_right (fun i _ -> f i) z.left () ; List.iter f z.right
(*
let exists tag f z =
  List.exists f (if tag then z.left else z.right)

let exists_left = exists true

let exists_right = exists false
*)
