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

open Cil_datatype
open Cil_types
    
let dkey_stmt = Mat_option.register_category "cil2assign:block_analyzer" 
let dkey_linear = Mat_option.register_category "cil2assign:linearizer" 

module Make = functor 
    (Assign : Poly_assign.S with type P.v = Varinfo.t
			    and type P.Var.Map.key = Varinfo.t 
			    and type P.Var.Set.t = Varinfo.Set.t) -> 
      struct 
	exception Loop_break of stmt
	 
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
            then 
              let assign = Assign.Assign (v,(exp_to_poly ~nd_var e)) in 
              let () = 
                Mat_option.debug ~dkey:dkey_stmt ~level:3 
                  "Assign generated : %a" 
                  Assign.pretty_assign assign in 
              Some assign
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
		    then raise (Loop_break s)
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
                  let out_of_loop_stmt = List.hd (Extlib.the break).succs in 
		  Some (Assign.Loop (block_to_body varinfo_used break s ([s;out_of_loop_stmt])))
		else 
		  Mat_option.abort 
		    "Non deterministic nested loop are not allowed"   
	      | Break _ -> raise (Loop_break s)
	      | Block _ | If _ -> None
              | _ -> 
	        Some (Assign.Other_stmt s)
		  
	and block_to_body
	    varinfo_used 
	    ?(nd_var = Varinfo.Map.empty) 
	    break
	    (head : Cil_types.stmt) 
            (last_stmts : Cil_types.stmt list) : Assign.body = 
 
	  let () = 
	    Mat_option.debug ~dkey:dkey_stmt "Block start : %a"
	      Printer.pp_stmt head in
	  let rec dfs stmt : (Assign.body) = 
	    Mat_option.debug ~dkey:dkey_stmt ~level:2
	      "Stmt %a studied" 
	      Stmt.pretty stmt
	    ;
	    if List.exists (Stmt.equal stmt) last_stmts
	    then 
	      begin  
		Mat_option.debug ~dkey:dkey_stmt ~level:3
		  "End of dfs."
		;
                []
	      end
	    else 
	      begin
		Mat_option.debug ~dkey:dkey_stmt ~level:3
		  "Stmt never seen";
              try
		  
                let poly_opt = stmt_to_poly_assign varinfo_used nd_var break stmt in
    
                let succs = 
                  match stmt.skind with
	            Cil_types.Loop(_,_,_,_,None) -> 
	              assert false
                  | Cil_types.Loop(_,_,_,_,Some s) -> s.succs
                  | _ -> 
                    stmt.succs in    
                
                let next_state = 
                  match stmt.skind with
                    If(e,b1,b2,_) -> 
                    let if_merge_stmt =
                      let rec last = 
                        function 
                        | ([],[]) -> (* Only reached when both then and else are empty from the 
                                        beginning *) 
                          Some (List.hd stmt.succs)

                        | (hd :: [],l) | (l,hd::[]) ->  
                          (* One of the list is empty *)
                          if (try (List.mem (List.hd hd.succs) last_stmts) with Failure _ -> true)
                          then match l with [] -> None | _ -> last ([],l)
                          else 
                            Some(List.hd hd.succs)
                        | ([],_ :: tl) | (_ :: tl,[]) -> last (tl,[])
                        | (_ :: tl1),(_ :: tl2) -> last (tl1,tl2)
                      in  
                      match last (b1.bstmts,b2.bstmts) with
                        Some s -> Some s
                      | _ -> (* At least one of the blocks have a terminal statement *)
                        let possible_succs = 
                          List.filter 
                            (fun s -> not(List.mem s b1.bstmts) && not(List.mem s b2.bstmts))
                            stmt.succs
                        in
                          match possible_succs with
                            [] -> None
                          | hd :: [] -> Some hd 
                          | _ -> assert false 
                    in                
                    
                    let res_post_if = 
                      match if_merge_stmt with 
                        None -> [] 
                      | Some s -> dfs s in
                    
                    let if_bdy b = 
                      let last_stmts = 
                        match if_merge_stmt with 
                        None -> last_stmts
                      | Some s -> s :: last_stmts in
                      match b.bstmts with 
                      [] -> []
                      | hd :: _ -> 
                        let bdy_less_first = 
                          block_to_body varinfo_used ~nd_var break hd last_stmts
                        in
                        match stmt_to_poly_assign  varinfo_used nd_var break hd  with 
                          None -> bdy_less_first
                        | Some s -> s :: bdy_less_first
                    in
                    
                    let bdy_b1 = if_bdy b1 and bdy_b2 =  if_bdy b2 in
  
                    Assign.Assert(e, bdy_b1, bdy_b2) :: res_post_if
                  | _ ->
                    begin 
                      assert (List.length succs = 1);
		      dfs (List.hd succs)
	            end 
                in
                match poly_opt with 
                    None -> next_state
                  | Some (Assign.Assert _) -> assert false; 
                    (* Assert treated by block_to_body *)
		  | Some (Assign.Assign (_,p) as aff) ->
		    Mat_option.debug ~dkey:dkey_stmt 
		      "Polynom generated : %a"
		      P.pp_print p;
		    
		    aff :: next_state 
		  | Some ((Assign.Loop _) as l) -> l :: next_state
		  | Some ((Assign.Other_stmt s) as l) -> 
                    Mat_option.debug ~dkey:dkey_stmt ~level:3
                      "No polynom generated from stmt %a"
	              Printer.pp_stmt s;
                    l :: next_state
		    
		with
		  Loop_break s-> [Assign.Other_stmt s]
	      end 
	  in
	  
	  let res = dfs (List.hd head.succs)
	  in
	  Mat_option.debug ~dkey:dkey_stmt ~level:5
	    "How many paths ? %i" (List.length res); res


 (**2.  Pilat2Cil *)
  
 let monom_to_var_memoizer : Cil_types.varinfo P.Monom.Hashtbl.t = P.Monom.Hashtbl.create 5

 let monom_to_var fundec typ (monom:P.Monom.t) : Cil_types.varinfo option = 
   if P.Monom.Hashtbl.mem monom_to_var_memoizer monom 
   then Some (P.Monom.Hashtbl.find monom_to_var_memoizer monom)
   else match P.deg_monom monom with 
       0 -> None
     | 1 -> Some (List.hd (P.to_var monom))
     | _ -> 
       let () = Mat_option.debug ~dkey:dkey_linear ~level:2 
           "Adding %s to the monom list"
           (P.Monom.varname monom) in
       let  var = 
       (Cil.makeLocalVar
         fundec 
         (P.Monom.pretty Format.str_formatter monom |> Format.flush_str_formatter)
         typ)
     in
     let () = P.Monom.Hashtbl.add monom_to_var_memoizer monom var 
     in Some var 
     

 let poly_to_linexp fundec typ loc poly = 
   let monoms = P.get_monomials poly in 
   let type_is_int = match typ with TInt _ -> true | TFloat _ -> false | _ -> assert false in
   Extlib.the 
     (P.Monom.Set.fold
        (fun m (acc_rval) -> 

           let var = monom_to_var fundec typ m in
           let poly_exp =                       
             let const = 
               try 
                 let coef = P.coef poly m |> P.R.t_to_float in
                 assert (not(type_is_int && floor coef <> coef));
                 Cil.new_exp 
                   ~loc 
                   (Const 
                      (if type_is_int 
                       then (CInt64 ((Integer.of_float coef),IInt,None))
                       else (CReal  (coef, FFloat, None))
                      )) 
               with
               Failure s ->
                 assert (s = "Poly.t_to_float");
                 Mat_option.fatal "Non deterministic linearization not available yet."
             in
             
             match var,acc_rval with 
               None,None -> const
             | None, Some a -> Cil.mkBinOp ~loc PlusA const a
             | Some var,_ ->  
               let var_exp = Cil.new_exp ~loc (Lval (Cil.var var)) in 
               let poly_part = Cil.mkBinOp ~loc Mult const var_exp in
               match acc_rval with 
                 None -> poly_part 
               | Some a -> Cil.mkBinOp ~loc PlusA poly_part a in 
           Some poly_exp
        )
        monoms
        None)
     

 (** Returns the cil statement corresponding to the polynomial assignment input *)
 let rec linassign_to_stmt blocks (kf:Kernel_function.t) typ loc assign = 
   let fundec = match kf.fundec with
       Definition(fundec,_) -> fundec 
     | Declaration _ -> assert false in
   match assign with
     Assign.LinLoop _ -> assert false (* TODO *)
   | Assign.LinOther_stmt s -> s (* TODO *)
   | Assign.LinAssert (e,b1,b2) -> 
     let f = block_linassign_to_block blocks kf typ loc in
     let skind = If(e, f b1, f b2, loc) in
     Cil.mkStmt ~ghost:false ~valid_sid:true skind
   | Assign.LinAssign (monom,poly) -> 
     let lval = Extlib.the (monom_to_var fundec typ monom) (* Can't be None, you can't assign 1 *) in
     let rval = poly_to_linexp fundec typ loc poly 
     in 
     let stmt = Cil.mkStmt ~ghost:false ~valid_sid:true (Instr (Set (Cil.var lval, rval, loc)))
     in
     let () = Kernel_function.register_stmt kf stmt blocks
     in stmt
     
 and block_linassign_to_block blocks (kf:Kernel_function.t) typ loc assigns = 
   let s_list = 
     List.fold_right
       (fun a acc_stmt -> 
          let stmt = linassign_to_stmt blocks kf typ loc a in
          stmt::acc_stmt
       ) 
       assigns
       []
   in Cil.mkBlockNonScoping s_list

  let export_variables () = 
    P.Monom.Hashtbl.fold
      (fun _ v acc -> v :: acc) 
      monom_to_var_memoizer
      []

  let initializers loc = 
    P.Monom.Hashtbl.fold
      (fun m v acc -> 
         let lval = Cil.var v in 
         
         let vars = P.to_var m in 
         let some_exp = 
           List.fold_left
             (fun acc v -> 
                match acc with 
                  None -> Some(Cil.new_exp ~loc (Lval (Cil.var v)))
                | Some exp -> Some (
                    (Cil.mkBinOp ~loc Mult exp (Cil.new_exp ~loc (Lval (Cil.var v))))
                  )
             )
             None 
             vars
         in
         Instr(Set(lval,(Extlib.the some_exp),loc)) :: acc)
      monom_to_var_memoizer
      []
      

end 
  
