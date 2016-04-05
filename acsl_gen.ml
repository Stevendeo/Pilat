(* Logic_const.new_code_annotation *)
open Cil_types
open Matrix_ast

let dkey_term = Mat_option.register_category "acsl_gen:term"  
let dkey_zterm = Mat_option.register_category "acsl_gen:zterm"  

module Var_cpt = State_builder.SharedCounter(struct let name = "pilat_counter" end)
let new_name () = Mat_option.NameConst.get () ^ (string_of_int (Var_cpt.next ()))

let to_code_annot (pred:predicate named) = 
  
  Logic_const.new_code_annotation (AInvariant ([],true,pred))

let term_node_is_zero tnode = 
  match tnode with
  | TConst (Integer (i,_)) -> i = Integer.zero
  | _ -> false

let monomial_to_mul_term m = 
  
  let rec __m_to_term vars = 
    match vars with
      [] -> Logic_const.term (TConst (Integer (Integer.one,(Some "1")))) Linteger
    | var :: [] -> 
      let lvar = Cil.cvar_to_lvar var in
      let term = 
	Logic_const.term 
	  (TLval 
	     (TVar lvar,TNoOffset)
	  ) 
	  Linteger
      in
      Mat_option.debug ~dkey:dkey_term ~level:3
	"End of the var list. Partial term generated : %a"
	Printer.pp_term term ;
      term
      
    | var :: tl -> 
      let lvar = Cil.cvar_to_lvar var in
      let tlval = Logic_const.term (TLval (TVar lvar,TNoOffset)) Linteger in
      let end_term =  __m_to_term tl in
      let res = 
	if term_node_is_zero end_term.term_node 
	then Logic_const.term (TConst (Integer (Integer.zero,(Some "0")))) Linteger
	else
	  
	  Logic_const.term (TBinOp (Mult,tlval,end_term)) Linteger
      in
      Mat_option.debug ~dkey:dkey_term ~level:3
	"Partial term generated : %a"
	Printer.pp_term res ;
      res
  in
  let res = 
  __m_to_term (F_poly.to_var m)
  in
  Mat_option.debug ~dkey:dkey_term ~level:2
    "Whole term generated : %a"
    Printer.pp_term res ;
  res

let vec_to_term (base:int Matrix_ast.F_poly.Monom.Map.t) (vec : Lacaml_D.vec) =
  let zero =  Logic_const.term (TConst (Integer (Integer.zero,(Some "0")))) Linteger
  in
  F_poly.Monom.Map.fold
    (fun monom row acc -> 
      if vec.{row} = 0. then acc else
      
      let logic_cst = 
	{ r_literal = string_of_float vec.{row};
	  r_nearest = vec.{row} ;
	  r_upper = vec.{row} ;    
	  r_lower = vec.{row} ;
	}
      in

      let term_cst = Logic_const.term (TConst (LReal logic_cst)) Linteger  in


      let monom_term = 
	
	Logic_const.term
	  (TBinOp
	     (Mult,
 	      term_cst,
	      monomial_to_mul_term monom)
	  ) Linteger 
	  
      in
      if acc = zero then monom_term else
      Logic_const.term (TBinOp (PlusA,acc,monom_term)) Linteger 
	
    )
    base
    zero

let vec_space_to_predicate
    (fundec: Cil_types.fundec)
    (base:int Matrix_ast.F_poly.Monom.Map.t) 
    (vec_list : Lacaml_D.vec list) 
    : predicate named =

  let zero =  (Logic_const.term (TConst (Integer (Integer.zero,(Some "0"))))) Linteger 
  in
  let term = 
    List.fold_left
      (fun acc vec -> 
	let term = vec_to_term base vec 
	in
	let new_ghost_var = Cil.makeLocalVar fundec (new_name ()) (TInt (IInt,[]))
	in
	new_ghost_var.vghost <- true;     
	let lvar = Cil.cvar_to_lvar new_ghost_var in
        let term_gvar = 
	  Logic_const.term
	    (TLval ((TVar lvar),TNoOffset)) Linteger 
	in
	let prod_term = 
	  Logic_const.term
	    (TBinOp
	       (Mult,
 		term_gvar,
		term) 
	    ) Linteger 
	    
	in
	if acc = zero then prod_term else 
	Logic_const.term
	   (TBinOp (PlusA,acc,prod_term)) Linteger 
	    
      )
      zero
      vec_list
  in
  let pred = 
    Prel
      (Req,
       term,
       zero)
  in
   
  Logic_const.unamed pred

let add_loop_annots kf stmt base vec_lists = 
  let fundec = match kf.fundec with
      Definition(f,_) -> f
    | Declaration _ -> assert false
  in
  let annots =   
    List.map 
      (fun vec -> 
	to_code_annot (vec_space_to_predicate fundec base vec)
      )
      vec_lists
      

  in
  List.iter (Annotations.add_code_annot Mat_option.emitter ~kf stmt) annots

(** Zarith *)

let vec_to_term_zarith (base:int Matrix_ast.F_poly.Monom.Map.t) (vec : Pilat_matrix.QMat.vec) =

  let () = Mat_option.debug ~dkey:dkey_zterm ~level:2
    "Vector given : %a" Pilat_matrix.QMat.pp_vec vec in
    

  let zero =  Logic_const.term (TConst (Integer (Integer.zero,(Some "0")))) Linteger
  in
  let vec_array = Pilat_matrix.QMat.vec_to_array vec in 
  F_poly.Monom.Map.fold
    (fun monom row acc -> 
      let row = row - 1 in
      assert (Z.equal Z.one (Q.den vec_array.(row)));
      let cst = vec_array.(row) in
      if Q.equal Q.zero cst then acc else
      
      let term_cst = 
	Logic_const.term (TConst (Integer (Integer.of_int (Q.to_int cst),(Some (Q.to_string cst))))) Linteger in


      let monom_term = 
	
	Logic_const.term
	  (TBinOp
	     (Mult,
 	      term_cst,
	      monomial_to_mul_term monom)
	  ) Linteger 
      
      in
      if acc = zero then monom_term else
      Logic_const.term (TBinOp (PlusA,acc,monom_term)) Linteger 
	
    )
    base
    zero


(** Returns a predicate based on the term list. Each term ei comes from a vector ei of the base of
    the invariant, so SUM(ki*ei) is the general invariant. *)
let term_list_to_predicate term_list fundec = 

  let zero =  (Logic_const.term (TConst (Integer (Integer.zero,(Some "0"))))) Linteger 
  in

  let term = 
    List.fold_left
      (fun acc term -> 
	let new_ghost_var = Cil.makeLocalVar fundec (new_name ()) (TInt (IInt,[]))
	in
	new_ghost_var.vghost <- true;     
	let lvar = Cil.cvar_to_lvar new_ghost_var in
        let term_gvar = 
	  Logic_const.term
	    (TLval ((TVar lvar),TNoOffset)) Linteger 
	in
	let prod_term = 
	  Logic_const.term
	    (TBinOp
	       (Mult,
 		term_gvar,
		term) 
	    ) Linteger 
	    
	in
	if acc = zero then prod_term else 
	Logic_const.term
	   (TBinOp (PlusA,acc,prod_term)) Linteger 
	    
      )
      zero
      term_list
  in
  let pred = 
    Prel
      (Req,
       term,
       zero)
  in
   
  Logic_const.unamed pred

exception No_zero_found of term
(** Searches if a term is never equals to zero with Value. Fails if so. *)
let value_search_of_non_zero term_list stmt=
  List.iter
    (fun t -> 
      let e = 
	!Db.Properties.Interp.term_to_exp ~result:None t in
      let vals = 
	!Db.Value.eval_expr 
	  ~with_alarms:CilE.warn_none_mode 
	  (Db.Value.get_stmt_state stmt)
	  e
      in
      if (Cvalue.V.contains_zero vals)
      then () 
      else (** This is never equal to 0, we can return*)
	raise (No_zero_found t)

    )
    term_list

(** Searches if a term does not contains variables. By construction, ther should not be 
    a term equal to 0. Fails if we find a term. *)

exception Var_found
let non_zero_search_from_scratch term_list =

  let var_visitor = 
    object
      inherit Visitor.frama_c_inplace
      method! vvrbl v = raise Var_found
    end
  in
  List.iter
    (fun t ->
      try 
	begin
	  ignore (Cil.visitCilTerm (var_visitor :> Cil.cilVisitor) t);
	  raise (No_zero_found t)
	end
      with   
	Var_found -> ()
    )
    term_list
(** Returns (Some t) if t is never equal to zero, None else*)
let test_never_zero (stmt : stmt) (term_list : term list) : term option =


  let if_no_zero_fails () = 
      if Db.Value.is_computed ()
      then
	value_search_of_non_zero term_list stmt
      else
	(** Naive search : looking for a term without not-constant variables *)
	non_zero_search_from_scratch term_list
	
  in

    try if_no_zero_fails (); None 
    with No_zero_found t -> Some t
	

let vec_space_to_predicate_zarith
    (fundec: Cil_types.fundec)
    (base:int Matrix_ast.F_poly.Monom.Map.t) 
    (vec_list : Pilat_matrix.QMat.vec list) 
    : predicate named =

  let term_list = 
    List.map
      (vec_to_term_zarith base) vec_list in

  (** If a term is always different to 0, then a stronger result is possible *)
  term_list_to_predicate term_list fundec
  

let add_loop_annots_zarith kf stmt base vec_lists = 
  let fundec = match kf.fundec with
      Definition(f,_) -> f
    | Declaration _ -> assert false
  in
  let annots =   
    List.map 
      (fun vecs -> 
	to_code_annot (vec_space_to_predicate_zarith fundec base vecs)
      )
      vec_lists
      
  in

  List.iter (Annotations.add_code_annot Mat_option.emitter ~kf stmt) annots

