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

open Cil_types
open Cil

(*open Logic_const
*)
let dkey_stmt = Mat_option.register_category "main:loop_analyser"
let dkey_vars = Mat_option.register_category "main:vars"
let dkey_time = Mat_option.register_category "main:timer"
let dkey_base = Mat_option.register_category "main:base"
let dkey_annot = Mat_option.register_category "main:annot"

let output_fun chan = Printf.fprintf chan "%s\n" 

let read_file chan =
  let lines = ref [] in
  try
    while true; do
      lines := input_line chan :: !lines
    done; 
    List.fold_right
      (fun str acc -> acc ^ str ^ "\n") 
      !lines 
      ""
  with End_of_file ->
    List.fold_right
      (fun str acc -> acc ^ str ^ "\n") 
      !lines 
      ""
(** Visitor *)

let loop_analyzer prj = 
  object(self)
    inherit Visitor.frama_c_copy prj

    val mutable new_variables = []
    method private add_vars vars = new_variables <- vars @ new_variables

    method! vfunc _ = 
      DoChildrenPost (
        fun f -> 
          Mat_option.debug ~dkey:dkey_vars "Adding: ";
          List.iter
            (fun v -> Mat_option.debug ~dkey:dkey_vars "%a" Cil_datatype.Varinfo.pretty v)
            new_variables;
          f.slocals <- new_variables @  f.slocals ; 
          f.sbody.blocals <- new_variables @ f.sbody.blocals  ; 
          new_variables <- []; 
          f)

    method! vstmt_aux stmt =
      let kf = Extlib.the self#current_kf in
      
      match stmt.skind with
      | Cil_types.Loop (annots,b,loc,conts,breaks) -> 

        let t_whole = Sys.time() in


        begin (* Loop treatment *)
	  let () = 	
            Mat_option.debug ~dkey:dkey_stmt "Loop %a studied"
              Cil_datatype.Stmt.pretty stmt;
            List.iter
              (fun s -> 
                 Mat_option.debug ~dkey:dkey_stmt ~level:5 "Stmt in loop = %a"
		   Cil_datatype.Stmt.pretty s;)
              b.bstmts
	  in

	  let (varinfos_used,nd_var) = Pilat_visitors.studied_variables b in
	  let num_variables = 
            Cil_datatype.Varinfo.Set.cardinal varinfos_used 
	  in

	  let () = Mat_option.debug ~dkey:dkey_stmt ~level:2 "Used varinfos computed";

	    Cil_datatype.Varinfo.Set.iter
              (fun v -> 
                 Mat_option.debug 
                   ~dkey:dkey_stmt 
                   ~level:3 
                   "Var %a" 
                   Printer.pp_varinfo v) varinfos_used in

	  let assign_is_deter = Cil_datatype.Varinfo.Map.is_empty nd_var
	  in
	  let (module Assign_type : 
                Poly_assign.S with type P.v = Cil_datatype.Varinfo.t
		               and type P.Var.Set.t = Cil_datatype.Varinfo.Set.t
	      ) = 


            match (Mat_option.Use_zarith.get ()), assign_is_deter with
              true,  true  -> (module Assign.Q_deterministic) 
            | true,  false -> 
              Mat_option.abort 
                "Use of zarith for non determinism is not implemented. \
                 If your program uses floating point numbers, consider the option -pilat-no-z."
            (*(module Assign.Q_non_deterministic) *)
            | false, true  -> (module Assign.Float_deterministic) 
            | false, false -> (module Assign.Float_non_deterministic)
	  in
	  (** 1st step : Computation of the block as a list of list of polynomials assignments. *)
	  let module Cil_parser = Cil2assign.Make(Assign_type) in
          let polys_opt = 
	    let out_of_loop_stmt =
            (Extlib.the breaks)
            in
            try 
              Some 
                (Cil_parser.block_to_body 
                   varinfos_used 
                   ~nd_var 
                   breaks 
                   stmt [stmt;out_of_loop_stmt])
	    with Poly_assign.Not_solvable -> None 
	  in
	  match polys_opt with 
            None -> 
            Mat_option.debug ~dkey:dkey_stmt "The loop is not solvable"; DoChildren

	  | Some body -> 
            Mat_option.debug ~dkey:dkey_stmt "The loop is solvable";


            Cil_datatype.Varinfo.Set.iter
              (fun v -> 
                 Mat_option.debug 
		   ~dkey:dkey_stmt 
		   ~level:3
		   "Var: %a" 
		   Printer.pp_varinfo v) varinfos_used ;

            Cil_datatype.Varinfo.Map.iter
              (fun v (f1,f2) -> 
                 Mat_option.debug 
		   ~dkey:dkey_stmt 
		   ~level:3 
		   "%a between %f and %f" 
		   Printer.pp_varinfo v f1 f2) nd_var ;
 
                 Mat_option.debug ~dkey:dkey_stmt ~level:5
                   "Assign: %a"
                   (Format.pp_print_list Assign_type.pretty_assign) body;

            let assigns,bases_for_each_loop = 
              Assign_type.add_monomial_modifications varinfos_used body

            in
            let base = Assign_type.monomial_base bases_for_each_loop 
            in
            let rev_base = Assign_type.reverse_base base in
            let matrices =  
 	           Assign_type.loop_matrix base assigns in

            let () = List.iter
                (fun mat -> 
                   Mat_option.debug ~dkey:dkey_stmt ~level:3
		     "Matrix generated : \n%a"
		     Assign_type.M.pp_print mat
                )
                matrices
            in
            if Mat_option.Prove.get () 
            then
              let () = Mat_option.feedback "Proving invariants" in
              let t_prove = Sys.time () in
              let open Property_status in
              let module Prover = Invar_prover.Make(Assign_type) in
              List.iter
                (fun annot ->
		   let status = 
	             Mat_option.debug ~dkey:dkey_annot
	               "Annotation : %a"
	               Printer.pp_code_annotation annot;
	             List.fold_left 
	               (fun acc mat -> 
	                  match acc with
			    False_and_reachable | False_if_reachable -> acc
	                  | Dont_know | True -> 
	                    begin
			      match Prover.prove_annot mat base annot with
		                True -> acc
			      | res -> res
	                    end
	               )
	               True
	               matrices
		   in

		   let () = 
	             Mat_option.feedback
	               "Invariant %a status : %s"
	               Printer.pp_code_annotation annot
	               (match status with
	                  True -> "True"
	                | Dont_know -> "?"
	                | _ -> "False") in

		   let emitter = Annotations.emitter_of_code_annot annot stmt 
		   and ip = Property.ip_of_code_annot_single kf stmt annot
		   in
		   Property_status.emit emitter ~hyps:[] ip status
                )
                (Annotations.code_annot stmt); 
              Mat_option.proof_timer := !Mat_option.proof_timer +. Sys.time () -. t_prove; 
              DoChildren
            else
              let () = Mat_option.feedback "Invariant generation" in
              let module Invariant_maker = Invariant_utils.Make(Assign_type) in
              let t_invar = Sys.time () in
              let whole_loop_invar = 
                List.fold_left
                  (fun acc (mat : Assign_type.mat) -> 
	             let () = 
	               Mat_option.debug ~dkey:dkey_stmt ~level:3 
	                 "New mat : %a" Assign_type.M.pp_print mat
	             in
 	             let invar = (Invariant_maker.invariant_computation assign_is_deter mat)
	             in 

	             Mat_option.debug ~dkey:dkey_stmt ~level:2 "Invar : ";
	             List.iteri
	               (fun i (limit,invars) -> 
	                  let () = 
			    Mat_option.debug ~dkey:dkey_stmt
		              "Invariant %s %i :" (Invariant_maker.lim_to_string limit)  (i + 1) in
	                  List.iter
			    (fun invar ->  
		               Mat_option.debug ~dkey:dkey_stmt
		                 "%a\n__"
		                 Assign_type.print_vec (rev_base,invar);					  
			    )invars;    
	               ) invar;


	             (mat,invar) :: acc

                  )
                  []
                  matrices
              in

              let () = Mat_option.invar_timer := !Mat_option.invar_timer +. Sys.time () -. t_invar in
              (** Redundancy analysis *)

              let t_redundancy = Sys.time ()
              in	
              let whole_loop_invar =
                if Mat_option.Redundancy.get () then
		  List.map
	            (fun (mat,invars) -> 
	               mat,List.map
	                 (fun (l,invar) ->
			    l,List.filter
		              (fun vec -> 
		                 not (Invariant_maker.redundant_invariant rev_base vec invar)
		              )
		              invar
	                 )
	                 invars 
	            )
	            whole_loop_invar
                else
		  whole_loop_invar
              in
              Mat_option.redun_timer := !Mat_option.redun_timer +. Sys.time () -. t_redundancy;

              let new_loop =   
                if Mat_option.Linearized_file.get () 
                then 
                  (* Check if the assignemnts satisfies the actual hypotheses : no nested loop nor 
                     conditions *) 
                  let test_loop =
                    List.for_all
                      (function
                        | Assign_type.LinLoop _ -> false 
                        |  _ -> true )
                  in
                  if not(test_loop assigns) then stmt 
                  else 
                    (* Builds the loop *)
 
                    let typ_is_int = 
                      Cil_datatype.Varinfo.Set.for_all
                        (fun v -> match v.vtype with TInt _ -> true | _ -> false) 
                        varinfos_used in
                    let typ = if typ_is_int then TInt(IInt,[]) else TFloat(FFloat,[])
                    in 
                    let blocks = 
                      (*try*) Kernel_function.find_all_enclosing_blocks stmt
                      (*with Not_found -> 
                        Mat_option.fatal "stmt %a not registered, cannot be found by kernel" 
                          Printer.pp_stmt stmt
                      *)in

                    let kf = 
                        (Extlib.the self#current_kf) in
                    let block = 
                      Cil_parser.block_linassign_to_block
                        blocks
                        kf
                        typ
                        loc
                        assigns
                    in
                    (** Get newly created variables to add to fundec locals *)
                    let monom_vars = (Cil_parser.export_variables()) in
                    let () = 
                      self#add_vars monom_vars
                    in

                    (** Now, creating the new loop *)
                    let new_loop = 
                      Cil.mkStmt
                        ~ghost:false
                        ~valid_sid:true
                        (Loop (annots,block,loc,conts,breaks)) in
                    let () = Kernel_function.register_stmt kf new_loop blocks in 
                    (** Initialize variables before the loop, done by Pilat_visitors *)
                    let init_list = Cil_parser.initializers loc in
                    List.iter (Pilat_visitors.register_stmt new_loop) init_list;
                    new_loop

                else stmt
              in

              let module Annot_generator = Acsl_gen.Make(Assign_type) in 

              let t_inter = Sys.time () in
              (** Intersecting the invariants if necessary *)
              if whole_loop_invar = [] then 
                if Mat_option.Linearized_file.get () 
                then DoChildrenPost (fun _ -> new_loop)
                else DoChildren 
              else if (assign_is_deter || List.length whole_loop_invar >= 2)
              then
                let invar_inter = 
                  List.fold_left
                    (fun acc (_,invar) -> 
                       if acc = Some [] then Some [] else
                         match acc with
	                   None -> Some invar
                         | Some l ->  
                           Some (Invariant_maker.intersection_invariants invar l))
                    None
                    whole_loop_invar 
                in
    
                
                let () = Mat_option.inter_timer := !Mat_option.inter_timer +. Sys.time () -. t_inter
                in 
                let () = 
                  Mat_option.whole_rel_time := Sys.time() -. t_whole +. !Mat_option.whole_rel_time 
                in
                let vars_to_add = Annot_generator.register_loop_annots
                    assign_is_deter
                    kf
                    new_loop
                    rev_base 
                    (Extlib.the invar_inter)
                    Cil_datatype.Varinfo.Map.empty
                    num_variables in
                self#add_vars vars_to_add;
                if Mat_option.Linearized_file.get () 
                then DoChildrenPost
                    (fun _ -> new_loop) 
                   else 
                     DoChildren
                else 
                  (* Non deterministic case *)
                  let mat,invar = List.hd whole_loop_invar
                  in 
                  let () = 
                    Mat_option.whole_rel_time := Sys.time() -. t_whole +. !Mat_option.whole_rel_time 
                  in
                  let () = 
                    self#add_vars
                      (Annot_generator.register_loop_annots
                      assign_is_deter
                      ~mat
                      kf
                      new_loop
                      rev_base
                      invar
                      nd_var
                      num_variables)
                  in 
                  if Mat_option.Linearized_file.get () 
                  then
                    DoChildrenPost
                      (fun _ ->  
                         
                         new_loop) 
                  else DoChildren

        end (* Loop *)
      | _ -> DoChildren 
  end

exception Size_error

let run_input_mat file = 
  let module Str_var : Pilat_math.Variable with type t = string = 
  struct 
    include Datatype.String
    let max _ = assert false
    let min _ = assert false
    let to_nvars _ = []
  end 
  in

  let (module A:Poly_assign.S with type P.v = string) = 
    if Mat_option.Use_zarith.get () 
    then (module 
           Poly_assign.Make 
             (Pilat_matrix.QMat) 
             (Poly.Make (Qring)(Str_var)))
    else (module Poly_assign.Make(Lacaml_matrix)(Poly.Make(Float)(Str_var)))
  in

  (** 1. Matrix parsing *)
  let chan = open_in file in 
  let str = really_input_string chan (in_channel_length chan) in 
  let matrices = Str.split (Str.regexp ";;") str in
  let matrices = 
    try 
      List.map
        A.M.of_str 
        matrices
    with A.M.Dimension_error _ -> raise Size_error
  in
  List.iter
    (fun mat -> 
       Mat_option.debug ~level:4
         "MATRIX\n%a\n\n"
         A.M.pp_print mat)
    matrices;

  (** 2. Variable management *)

  let vars = Str.split (Str.regexp ":") (Mat_option.Var_focus.get ()) in 
  let i = ref 0 in 
  let var_map = 
    List.fold_left
      (fun acc v -> 
         let new_acc = 
           A.Imap.add !i (A.P.var_to_monom v) acc
         in
         i := !i + 1; new_acc)
      A.Imap.empty
      vars
  in

  (** 2.5 Tests *)
  let mat_size = A.M.get_dim_col (List.hd matrices) in
  let all_same_size = 
    List.for_all 
      (fun mat -> 
         let cols = A.M.get_dim_col mat 
         in cols == A.M.get_dim_row mat && cols == mat_size)
      matrices in
  if (not all_same_size)
  then raise Size_error;

  (** 3. Invariant computation *)

  let module I = Invariant_utils.Make (A)
  in
  let first_invar = I.invariant_computation false (List.hd matrices)
  in
  let invars = 
    List.fold_left
      (fun acc mat -> 
         I.intersection_invariants
           acc
           (I.invariant_computation false mat)
      )
      first_invar
      (List.tl matrices) in


  (** 4. Invariant as polynomials *)

  List.iter
    (fun (limit,inv) -> 
       Mat_option.feedback "%s :\n----\n" (I.lim_to_string limit);
       List.iter
         (fun vec -> 
            let p = 
              I.vec_to_poly 
                var_map 
                vec in 
            Mat_option.feedback "\n%a\n--" A.P.pp_print p) inv;       
       Mat_option.feedback "--"
    )
    invars
let run () =  
  if Mat_option.Enabled.get () 
  then
    let () = 
      Mat_option.Enabled.set false;
      Mat_option.feedback
        "Welcome to Frama-C's Pilat invariant generator"
    in 
    let mat_input =  Mat_option.Mat_input.get ()  in 
    if mat_input <> "" then 
      begin
        try
          run_input_mat mat_input
        with
          Size_error -> 
          Mat_option.feedback "Not all matrices have the same size or are not squared." 
      end 
    else 

      let file = Ast.get () 
      in  
      let filename = 
        let fname = Mat_option.Output_C_File.get () in 

        if  fname = "" then file.fileName ^ "_annot.c" else fname
      in
      List.iter
        (function
          |GFun (f,_) -> 
            Cfg.prepareCFG f; (* Registers break points of loops *)
            Cfg.clearCFGinfo f; (* Prepares cfgFun *)
            Cfg.cfgFun f;(* Sets the correct break statements in loops *)
            
          | _ -> ())
        file.globals;
      Kernel_function.clear_sid_info (); (* Clears kernel_functions informations, 
                                            will be recomputed automatically. *)

      let lin_prj = 

        File.create_project_from_visitor "tmp_project" loop_analyzer
      in 
      let () = Project.set_current lin_prj in

      Acsl_gen.emit_annots ();
        List.iter
        (function
          |GFun (f,_) -> 
            Cfg.clearCFGinfo f; (* Prepares cfgFun *)
            Cfg.cfgFun f;(* Necessary for the next visitor *)
            
          | _ -> ())
        file.globals;
      let prj =
        File.create_project_from_visitor 
          ~last:true
          "new_pilat_project" 
          (fun p -> new Pilat_visitors.fundec_updater p) 
      in
      (*List.iter
        (function
        |GFun (f,_) -> 
          File.must_recompute_cfg f;
        | _ -> ())
        file.globals;
      *)
      Mat_option.debug ~dkey:dkey_time 
        "Time to compute the relations : %f" ! Mat_option.whole_rel_time ;

      Mat_option.debug ~dkey:dkey_time ~level:2
        "Invariant generation time : %f\nIntersection time : %f\nNullspace time %f\nEigenvalues : %f\n Char. poly : %f\nRedundancy analysis : %f" 
        !Mat_option.invar_timer 
        !Mat_option.inter_timer 
        !Mat_option.nullspace_timer
        !Mat_option.ev_timer
        !Mat_option.char_poly_timer
        !Mat_option.redun_timer
      ;

      if not(Mat_option.Prove.get ()) then 
        let cout = open_out filename in
        let fmt = Format.formatter_of_out_channel cout in
        Kernel.Unicode.without_unicode
          (fun () ->
             File.pretty_ast ~prj ~fmt ();
             close_out cout;
             Mat_option.feedback "C file generation      : done\n";
          ) ()



let () = Db.Main.extend run
