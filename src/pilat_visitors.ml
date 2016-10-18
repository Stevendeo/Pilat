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

let print_stmt_list sl = 
Mat_option.debug ~dkey:dkey_stmt ~level:4
  "\nBEGIN\n";
  List.iter
    (fun s -> Mat_option.debug ~dkey:dkey_stmt ~level:4
      "-- %a" Printer.pp_stmt s
    )sl;
    
Mat_option.debug ~dkey:dkey_stmt ~level:4
  "\nEND\n";
class fundec_updater prj = 
object(self)
  inherit (Visitor.frama_c_copy prj)
    
   (* TODO : There is still a problem, after the stmt is added to the cfg the cfg is unusable
     for other tools*) 

  (*method! vfunc _ =
    DoChildrenPost (fun f -> let () = Cfg.clearCFGinfo f in f) 
 
  method! vfile _ = DoChildrenPost (fun f -> let () = Cfg.clearFileCFG f in f)*) 
  method! vvdec v = ChangeToPost (v, fun v -> v)
  method! vvrbl v = ChangeToPost (v, fun v -> v)

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
      let succ = List.hd s.succs in
      let new_stmtkinds = Cil_datatype.Stmt.Hashtbl.find stmt_init_table succ
      in
      let () = Mat_option.debug ~dkey:dkey_stmt ~level:2 
	"Statement %a has statements to add."
	Printer.pp_stmt s;
       Cil_datatype.Stmt.Hashtbl.remove stmt_init_table succ in
      
      let s_list,_ = 
	List.fold_left
	  (fun (acc_stmts, next_stmt) new_stmtkind -> 
	    
	    let stmt = 
	      Cil.mkStmt(*Cfg ~ref_stmt:next_stmt ~before:true ~*)
		~valid_sid:true 
		new_stmtkind in
	    
	    stmt.ghost <- true;
	    next_stmt.succs <- [stmt];
	    (*next_stmt.preds <- stmt :: next_stmt.preds;*)
	    let () = Mat_option.debug ~dkey:dkey_stmt 
	      "Adding stmt %a of id %i to the cfg after %a" 
	      Printer.pp_stmt stmt stmt.sid Printer.pp_stmt next_stmt;
	      
	    in (stmt::acc_stmts,stmt))
	  ([],s)
	  new_stmtkinds
      in

      let new_block = 
	Cil.mkStmt ~ghost:false ~valid_sid:true
	  (Block
	     {battrs = [];
	      blocals = [];
	      bstmts =  s::s_list
	     }
	  )
      in
      (*fundec.sallstmts <- new_block :: fundec.sallstmts;*)
      let rec fundec_stmt_zipper left right = 
	match right with
	  [] -> raise Not_found
	| hd :: tl -> 
	  let () = 
	    Mat_option.debug ~dkey:dkey_stmt ~level:5
	      "Does %i = %i ? %b"
	      hd.sid s.sid (hd.sid = s.sid) in
	  if Cil_datatype.Stmt.equal hd s
	  then 
	    (* The first statement of left is the statement before the loop *)
	    let () = 
	      Mat_option.debug ~dkey:dkey_stmt ~level:2
		"Adding %a to Cfg after %a"
		Printer.pp_stmt (List.hd s_list)
		Printer.pp_stmt (List.hd left);

	      (List.hd left).succs <- [(List.hd s_list)];

	    
	    (* This is where we insert s_list in the fundec body *)

	      Mat_option.debug ~dkey:dkey_stmt ~level:2
		"Adding the statement list to the fundec" in
	      let res = ((List.rev left) @ (s::new_block::s_list@(List.tl right))) in 
	      print_stmt_list res;
	      res 

	  else fundec_stmt_zipper ((List.hd right)::left) tl
      in
      let () = 
	Mat_option.debug ~dkey:dkey_stmt ~level:3
	  "Search of %a.%i in" Cil_datatype.Stmt.pretty s s.sid;
	List.iter 
	  (fun s -> 
	    Mat_option.debug ~dkey:dkey_stmt ~level:3
	      "-- %a.%i\n" Cil_datatype.Stmt.pretty s s.sid)
	  fundec.sbody.bstmts in
      let () = 
	try 
	  fundec.sbody.bstmts <- fundec_stmt_zipper [] fundec.sbody.bstmts;
	  fundec.sallstmts <- fundec_stmt_zipper [] fundec.sallstmts
	with Not_found -> 
	  Mat_option.feedback 
	    "Statement %a not in fundec. Problem in CFG ?" Cil_datatype.Stmt.pretty s
      in
      ChangeDoChildrenPost (new_block, fun i -> i)
  
    with 
      Not_found (* Stmt.Hashtbl.find stmt_init_table s *) -> DoChildren
    | Failure "hd" -> DoChildren
     
end
