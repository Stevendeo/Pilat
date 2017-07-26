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

open Cil_datatype
open Cil_types

let dkey_stmt = Mat_option.register_category "cil2assign:block_analyzer" 

module Make = functor 
    (Assign : Poly_assign.S with type P.v = Varinfo.t
			    and type P.Var.Map.key = Varinfo.t 
			    and type P.Var.Set.t = Varinfo.Set.t) -> 
      struct 
	exception Loop_break 
	 
	module P = Assign.P
(*	module R = P.R *)


	let poly_hashtbl = Cil_datatype.Stmt.Hashtbl.create 12

	let non_det_var_memoizer = Varinfo.Hashtbl.create 2

	let exp_to_poly ?(nd_var=Varinfo.Map.empty) exp =
	  let float_of_const c = 
	    match c with
	      CInt64 (i,_,_) -> Integer.to_float i
	    | CChr c -> Integer.to_float (Cil.charConstToInt c)
	    | CReal (f,_,_) -> f
	    | _ -> assert false
	  in
	  let rec __e_to_p e = 
	    match e.enode with 
	      Const c -> 
		P.const (c |> float_of_const |> P.R.float_to_t)
	    | Lval (Var v,_) ->
	      begin
		try 
		  P.const (Varinfo.Hashtbl.find non_det_var_memoizer v)
		with Not_found -> 
		  try 
		    let (low,up) = Varinfo.Map.find v nd_var 
		    in
		    
		    let new_rep = (P.R.non_det_repr low up) in 
		    
		    
		    let () = 
		      Mat_option.debug ~dkey:dkey_stmt ~level:2
			"Variable %a non deterministic, first use. Representant : %a"
			Varinfo.pretty v 
			P.R.pp_print new_rep
		    in
		    
		    let () = Varinfo.Hashtbl.add non_det_var_memoizer v new_rep
		    in
		    P.const new_rep
		  with
		    Not_found (* Varinfo.Map.find *) -> 
		      P.monomial P.R.one [v,1]
	      end
	    | Lval _ -> assert false   
	    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> assert false
	    | UnOp (Neg,e,_) -> 
	      P.sub P.zero (__e_to_p e)
	    | UnOp _ -> assert false
	    | BinOp (binop,e1,e2,_) -> 
	      begin
		match binop with
		  PlusA | PlusPI | IndexPI -> P.add (__e_to_p e1) (__e_to_p e2)
		| MinusA | MinusPI | MinusPP -> P.sub (__e_to_p e1) (__e_to_p e2)
		| Mult -> P.mul (__e_to_p e1) (__e_to_p e2)
		| Div -> 
		  begin 
		    match e2.enode with
		      Const c -> 
			P.mul 
			  (__e_to_p e1) 
			  (P.const (1. /.(c |> float_of_const) |> P.R.float_to_t))
		    | _ ->
		      Mat_option.abort 
			"The expression %a is a forbidden division." 
			Printer.pp_exp exp
		  end	    
		| _ -> assert false
	      end
	    | CastE (_,e) -> __e_to_p e
	    | _ -> assert false
	  in
	  __e_to_p exp
	    
	let instr_to_poly_assign varinfo_used nd_var : Cil_types.instr -> Assign.t option = 
	  function
	  | Set ((Var v, _),e,_) -> 
	    if P.Var.Set.mem v varinfo_used 
	    then Some (Assign.Assign (v,(exp_to_poly ~nd_var e)))
	    else None 
	  | Call(_,{enode = Lval(Var v,NoOffset) },_,_) as i -> 
	    if (v.vorig_name = Mat_option.non_det_name) then None
	    else 
	      let () = Mat_option.feedback
		"Call %a not supported, assuming no effect"
		Printer.pp_instr i in
	      None
	  | Skip _ -> None
	  | i -> 
	    Mat_option.abort
	      "Instruction %a not supported"
	      Printer.pp_instr i
	      
	let register_poly = Cil_datatype.Stmt.Hashtbl.replace poly_hashtbl   
	  
	let rec stmt_to_poly_assign varinfo_used nd_var break s : Assign.t option = 
	  
	  try 
	    Stmt.Hashtbl.find poly_hashtbl s 
	  with 
	    Not_found -> 
	      match s.skind with
		Instr i -> 	
		  let () = Mat_option.debug ~dkey:dkey_stmt
		    "Instruction" in
		  if break = None then None 
		  else 
		    if Stmt.equal s (Extlib.the break)
		    then raise Loop_break
		    else 
		      let () = Mat_option.debug ~dkey:dkey_stmt
			"Instruction"
		      in
		      begin
			match instr_to_poly_assign varinfo_used nd_var i with 
			  Some p -> 
			    register_poly s (Some p); 
			    Some p
			| None -> register_poly s None; None
		      end
	      | Cil_types.Loop (_,b,_,_,break) -> 
		
		if Varinfo.Map.is_empty nd_var
		then 
		  let () =
		    Mat_option.debug ~dkey:dkey_stmt
		      "Nested loop";
		    List.iter
		      (fun s -> 
			Mat_option.debug ~dkey:dkey_stmt ~level:2
			  "-- %a"
			  Printer.pp_stmt s)
		      b.bstmts
		    ;
		    
		  in
		  Some (Assign.Loop (block_to_poly_lists varinfo_used break b))
		else 
		  Mat_option.abort 
		    "Non deterministic nested loop are not allowed"   
	      | Break _ -> raise Loop_break
	      | _ -> 
		None
		  
	and block_to_poly_lists 
	    varinfo_used 
	    ?(nd_var = Varinfo.Map.empty) 
	    break
	    block : Assign.body list = 
	  let head = 
	    try 
	      List.hd 
		(List.hd block.bstmts).preds with Failure _ ->  
		  Mat_option.feedback 
		    "Error incoming : head of the block is %a" Printer.pp_stmt (List.hd block.bstmts);
		  
		  failwith "hd"
	  (* It must be the entry of the loop *)
	  in
	  let () = 
	    Mat_option.debug ~dkey:dkey_stmt "Loop head : %a"
	      Printer.pp_stmt head in
	  let rec dfs stmt = 
	    Mat_option.debug ~dkey:dkey_stmt ~level:2
	      "Stmt %a studied" 
	      Stmt.pretty stmt
	    ;
	    if Stmt.equal stmt head
	    then 
	      begin  
		Mat_option.debug ~dkey:dkey_stmt ~level:3
		  "Stmt already seen : loop." 
		;
		[[]] 
	      end
	    else 
	      begin
		Mat_option.debug ~dkey:dkey_stmt ~level:3
		  "Stmt never seen" 
		;
		
		try
		  
		  let poly_opt = stmt_to_poly_assign varinfo_used nd_var break stmt in
		  
		  let succs = 
		    match stmt.skind with
		      Cil_types.Loop(_,_,_,_,None) -> 
			assert false
		    | Cil_types.Loop(_,_,_,_,Some s) -> s.succs
		    | _ -> 
		      stmt.succs in    
		  
		  let future_lists = 
		    List.fold_left
		      (fun acc succ -> 		
			Mat_option.debug ~dkey:dkey_stmt ~level:4
			  "Successor of %a analyzed :\n %a"
			  Printer.pp_stmt stmt
			  Printer.pp_stmt succ;
			(dfs succ) @ acc)
		      []
		      succs
		  in
		  Mat_option.debug ~dkey:dkey_stmt ~level:3
		    "List of paths : %i" (List.length future_lists) ;
		  let (++) elt l = List.map (fun li -> elt :: li) l
		  in
		  match poly_opt with 
		    None -> 
		      Mat_option.debug ~dkey:dkey_stmt ~level:3
			"No polynom generated from stmt %a"
			Printer.pp_stmt stmt
		      ;
		      future_lists
		  | Some (Assign.Assign (_,p) as aff) ->
		    Mat_option.debug ~dkey:dkey_stmt 
		      "Polynom generated : %a"
		      P.pp_print p;
		    
		    aff ++ future_lists
		  | Some ((Assign.Loop _) as l) -> l ++ future_lists
		    
		with
		  Loop_break -> []
	      end 
	  in
	  
	  let res = dfs (List.hd head.succs)
	  in
	  Mat_option.debug ~dkey:dkey_stmt ~level:5
	    "How many paths ? %i" (List.length res); res
	      
	      
	      
      end 
  
