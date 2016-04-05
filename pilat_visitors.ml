open Cil_types
open Cil_datatype
open Cil

(** Returns the varinfos used in the block in argument *)
let varinfo_registerer block = 
  let vinfos = ref Cil_datatype.Varinfo.Set.empty in
  
  let visitor = 
    object
      
      inherit Visitor.frama_c_inplace
      method! vvrbl v = 
	let () = vinfos := Cil_datatype.Varinfo.Set.add v !vinfos
	in
	SkipChildren      
      method! vstmt s = 
	match s.skind with 
	  If _ -> Cil.SkipChildren 
	| _ -> Cil.DoChildren
    end 
  in
  let () = 
    ignore (Cil.visitCilBlock (visitor :> Cil.cilVisitor) block)
  in
  !vinfos

let stmt_init_table = Cil_datatype.Stmt.Hashtbl.create 42

let register_stmt = Cil_datatype.Stmt.Hashtbl.add stmt_init_table 

class fundec_updater = 
object
  inherit Visitor.frama_c_inplace
  method! vfunc fundec = 
    List.iter 
      (fun ref_stmt -> 
	try 
	  let new_stmtkind = Cil_datatype.Stmt.Hashtbl.find stmt_init_table ref_stmt
	  in
	  let new_stmt = mkStmtCfg ~before:true ~new_stmtkind ~ref_stmt 
	  in
	  new_stmt.ghost <- true;
	  fundec.sallstmts <- new_stmt :: fundec.sallstmts
	with 
	  Not_found -> ()
      )
      fundec.sallstmts;
    ChangeTo fundec
     
end
    
