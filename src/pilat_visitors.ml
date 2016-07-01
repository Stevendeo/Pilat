open Cil_types
open Cil

let dkey_stmt = Mat_option.register_category "pilat_vis:stmt"

(** Returns the varinfos used in the block in argument *)
let varinfo_registerer block = 
  let vinfos = ref Cil_datatype.Varinfo.Set.empty in
  
  let focused_vinfo = Mat_option.var_list ()
  in
  let visitor = 
object(self)
      inherit Visitor.frama_c_inplace
      
	
      method! vvrbl v = 
	match self#current_stmt with 
	  None -> DoChildren (* This case might be useless *)
	| Some {skind = If _ } -> DoChildren
	| _ -> 
	  let () = vinfos := Cil_datatype.Varinfo.Set.add v !vinfos
	  in
	  SkipChildren      

    end 
  in
  let () = 
    ignore (Cil.visitCilBlock (visitor :> cilVisitor) block)
  in
  if Cil_datatype.Varinfo.Set.is_empty focused_vinfo
  then
    !vinfos
  else
    Cil_datatype.Varinfo.Set.inter !vinfos focused_vinfo

let stmt_init_table = Cil_datatype.Stmt.Hashtbl.create 42

let loop_annot_table = Cil_datatype.Stmt.Hashtbl.create 42


let register_stmt loop_stmt init =
  let old_bind = 
    try 
      Cil_datatype.Stmt.Hashtbl.find stmt_init_table loop_stmt 
    with 
      Not_found -> [] in 
  Cil_datatype.Stmt.Hashtbl.replace stmt_init_table loop_stmt (init :: old_bind)

let register_annot_list table loop_stmt annots = 
    let old_bind = 
    try 
      Cil_datatype.Stmt.Hashtbl.find table loop_stmt 
    with 
      Not_found -> [] in 
  Cil_datatype.Stmt.Hashtbl.replace table loop_stmt (annots@old_bind)

let register_annot = register_annot_list loop_annot_table

class fundec_updater prj = 
object(self)
  inherit (Visitor.frama_c_copy prj)
    
   (* TODO : There is still a problem, after the stmt is added to the cfg the cfg is unusable
     for other tools*) 

  (*method! vfunc _ =
    DoChildrenPost (fun f -> let () = Cfg.clearCFGinfo f in f) 
 
  method! vfile _ = DoChildrenPost (fun f -> let () = Cfg.clearFileCFG f in f)*) 
      
  method! vstmt_aux s = 
    let kf = (Extlib.the self#current_kf) in
    let fundec = match kf.fundec with
	  Definition (f,_) -> f
	| Declaration _ -> assert false in

    let () = (* Adding annotations *)
      try 
	let annots = Cil_datatype.Stmt.Hashtbl.find loop_annot_table s in
	let () = Cil_datatype.Stmt.Hashtbl.remove loop_annot_table s in
	List.iter (
	  fun annot -> 
	    let () = Annotations.add_code_annot Mat_option.emitter ~kf s annot 
	    in 
	    let ip = Property.ip_of_code_annot_single kf s annot in 
	    Property_status.emit Mat_option.emitter ~hyps:[] ip Property_status.True
	)annots
     
      with Not_found (* Stmt.Hashtbl.find loop_annot_table s *) -> ()
    in
    try 
      let new_stmtkinds = Cil_datatype.Stmt.Hashtbl.find stmt_init_table s 
      in
      
      let () = Cil_datatype.Stmt.Hashtbl.remove stmt_init_table s in
      
      let s_list = 
	List.map
	  (fun new_stmtkind -> 	  
	    let stmt = Cil.mkStmtCfg ~ref_stmt:s ~before:false ~new_stmtkind in
	    stmt.ghost <- true;
	    let () = Mat_option.debug ~dkey:dkey_stmt 
	      "Adding stmt %a to the cfg before %a" 
	      Printer.pp_stmt stmt Printer.pp_stmt s
	      
	    in stmt)
	  new_stmtkinds
      in

      let () = 
	fundec.sallstmts <- s_list@fundec.sallstmts;
      in 
      
      let new_block = 
	Cil.mkStmt ~ghost:false ~valid_sid:true
	  (Block
	     {battrs = [];
	      blocals = [];
	      bstmts =  (s_list@[s])
	     }
	  )
      in
      let rec fundec_stmt_zipper left right = 
	match right with
	  [] -> assert false
	| hd :: tl -> 
	  if Cil_datatype.Stmt.equal hd s
	  then fundec.sbody.bstmts <- ((List.rev left) @ (s_list@right))
	  else fundec_stmt_zipper ((List.hd right)::left) tl
      in
      
      let () = fundec_stmt_zipper [] fundec.sbody.bstmts 
      in
      ChangeDoChildrenPost (new_block, fun i -> i)
  
    with Not_found (* Stmt.Hashtbl.find stmt_init_table s *) -> DoChildren
   
     
end
