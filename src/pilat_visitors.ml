open Cil_types
open Cil_datatype
open Cil

let dkey_fundec = Mat_option.register_category "pilat_vis:fundec"

(** Returns the varinfos used in the block in argument *)
let varinfo_registerer block = 
  let vinfos = ref Cil_datatype.Varinfo.Set.empty in
  
  let visitor = 
object(self)
      inherit Visitor.frama_c_inplace
      method! vvrbl v = 
	match self#current_stmt with 
	  None -> DoChildren (* This case might be useless *)
	| Some {skind = If _ } -> DoChildren
	| _ -> 
	  let () = vinfos := Varinfo.Set.add v !vinfos
	  in
	  SkipChildren      

    end 
  in
  let () = 
    ignore (Cil.visitCilBlock (visitor :> cilVisitor) block)
  in
  !vinfos

let stmt_init_table = Stmt.Hashtbl.create 42

let register_stmt loop_stmt init =
  let old_bind = 
    try 
      Stmt.Hashtbl.find stmt_init_table loop_stmt 
    with 
      Not_found -> [] in 
  Stmt.Hashtbl.replace stmt_init_table loop_stmt (init :: old_bind)

class fundec_updater prj = 
object
  inherit (Visitor.frama_c_copy prj)
  method! vfunc fundec = 
    List.iter 
      (fun ref_stmt -> 
	try 
	  let new_stmtkinds = Stmt.Hashtbl.find stmt_init_table ref_stmt
	  in
	  List.iter
	    (fun new_stmtkind -> 
	      
	      let new_stmt = Cil.mkStmtCfg ~before:false ~new_stmtkind ~ref_stmt 
	      in
	      
	      let () = 
		Mat_option.debug ~dkey:dkey_fundec "Adding %a to the CFG before %a" 
		  Printer.pp_stmt new_stmt
		  Printer.pp_stmt ref_stmt
	      in
	      
	      new_stmt.ghost <- true;
	      let rec fundec_stmt_zipper left right = 
		match right with
		  [] -> assert false
		| hd :: tl -> 
		  if Stmt.equal hd ref_stmt
		  then fundec.sbody.bstmts <- ((List.rev left) @ (new_stmt:: right))
		  else fundec_stmt_zipper ((List.hd right)::left) (List.tl right)
	      in
	      
	      fundec_stmt_zipper [] fundec.sbody.bstmts 
	    )
	    new_stmtkinds
	with 
	  Not_found -> ()
      )
      fundec.sallstmts;
    ChangeDoChildrenPost (fundec,(fun i -> i))
     
end
