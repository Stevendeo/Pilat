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

let dkey_var = Mat_option.register_category "pilat_vis:varinfo_reg"
let dkey_stmt = Mat_option.register_category "pilat_vis:stmt"

let float_of_const c =
  match c with
    CInt64 (i,_,_) -> Integer.to_float i
  | CChr c -> Integer.to_float (Cil.charConstToInt c)
  | CReal (f,_,_) -> f
  | _ -> assert false

let rec arg_exp e =
  match e.enode with
    Const c -> float_of_const c
  | CastE (_,e) | Info (e,_) -> arg_exp e
  | UnOp (Neg,e,_) -> -1.*.  (arg_exp e)
  | _ -> Mat_option.abort "Bad argument for non det function : %a."
    Printer.pp_exp e

(** Returns the varinfos used in the block in argument *)
let studied_variables block =
  let vinfos = ref Cil_datatype.Varinfo.Set.empty in

  let focused_vinfo = Mat_option.var_list ()
  in
  let non_det_variables = ref Cil_datatype.Varinfo.Map.empty
  in
  let visitor =
    object(self)
      inherit Visitor.frama_c_inplace

      method! vvrbl v =

	match self#current_stmt with
	  None -> DoChildren (* This case might be useless *)
	| Some {skind = If _ } -> DoChildren
	| Some ({skind = Instr (Call(Some(Var nd,_),{ enode = Lval(Var v,NoOffset) },args,_))} as s)->
	  let () =
	    if v.vorig_name = Mat_option.non_det_name && (List.length args) = 2
	    then
	      let () = Mat_option.debug ~dkey:dkey_var
		"Non deterministic call : %s"
		v.vorig_name in

	      let fst_arg = List.hd args and snd_arg = List.hd (List.tl args) in
	      non_det_variables :=
		Cil_datatype.Varinfo.Map.add
		nd
		((arg_exp fst_arg),arg_exp snd_arg)
		!non_det_variables
	    else
	      if Cil_datatype.Varinfo.Set.mem v focused_vinfo then

		Mat_option.abort "Function call %a in the loop modifying a studied variable : undefined behavior."
		  Printer.pp_stmt s
	  in
	  DoChildren
	| Some ({skind = Instr (Call (Some (Var v,_),_,_,_))}as s) ->
	  if Cil_datatype.Varinfo.Set.mem v focused_vinfo then

	  Mat_option.abort "Function call %a in the loop modifying a studied variable : undefined behavior."
	    Printer.pp_stmt s

	  else DoChildren

	| Some {skind = Instr (Call _)} ->
	  Mat_option.feedback
             "Function call in the loop without assignment : assert it does nothing.";
          DoChildren;
	| s ->
	  let () = Mat_option.debug ~dkey:dkey_var
	    "Variable %s added by statement %a"
	    v.vorig_name
	    Printer.pp_stmt (Extlib.the s)
	  in
	  let () = vinfos := Cil_datatype.Varinfo.Set.add v !vinfos
	  in
	  SkipChildren

    end
  in
  let () =
    ignore (Cil.visitCilBlock (visitor :> cilVisitor) block)
  in
  let var =
    if Cil_datatype.Varinfo.Set.is_empty focused_vinfo
    then
      !vinfos
    else
      Cil_datatype.Varinfo.Set.inter !vinfos focused_vinfo
  in
  var,!non_det_variables

let stmt_init_table = Cil_datatype.Stmt.Hashtbl.create 42

let register_stmt loop_stmt init =
  let old_bind =
    try
      Cil_datatype.Stmt.Hashtbl.find stmt_init_table loop_stmt
    with
      Not_found -> [] in
  Cil_datatype.Stmt.Hashtbl.replace stmt_init_table loop_stmt (init :: old_bind)

let make_ghost_assign skind =
  let stmt =
    Cil.mkStmt(*Cfg ~ref_stmt:next_stmt ~before:true ~*)
      ~valid_sid:true
      skind in
  let is_ghost =
    match skind with
      Instr(Set((Var v,_),_,_)) -> v.vghost
    | _ -> assert false (* Now, there are only Instr added by Pilat visitors *)
  in
  stmt.ghost <- is_ghost;
  stmt

let make_assign_block skinds next_stmt =
  let s_list,_ =
	List.fold_left
	  (fun (acc_stmts, next_stmt) new_stmtkind ->
                 let stmt = make_ghost_assign new_stmtkind in
                 let () = Mat_option.debug ~dkey:dkey_stmt
	             "Adding stmt %a of id %i to the cfg before %a"
	             Printer.pp_stmt stmt stmt.sid Printer.pp_stmt next_stmt in
	         next_stmt.preds <- [stmt];
                 stmt.succs <- [next_stmt];
	         (stmt::acc_stmts,stmt))
	  ([],next_stmt)
	  skinds
  in
  let s_list = List.rev(next_stmt::s_list) in
  let () =
    Mat_option.debug ~dkey:dkey_stmt ~level:4
      "New block is %a"
      (Format.pp_print_list ~pp_sep: (fun fmt _ -> Format.fprintf fmt "\n") Printer.pp_stmt) s_list in
  Cil.mkStmt ~ghost:false ~valid_sid:true
    (Block
       {
         bscoping = false;
         battrs = [];
         blocals = [];
         bstatics = [];
         bstmts =  s_list
       }
    )
 (*
class fundec_updater prj =
object
  inherit (Visitor.frama_c_copy prj)

  method! vstmt_aux s =
    try
      let succ = List.hd s.succs in
      let new_stmtkinds = Cil_datatype.Stmt.Hashtbl.find stmt_init_table succ
      in
      let () = Mat_option.debug ~dkey:dkey_stmt ~level:2
	"Statement %a has statements to add."
	Printer.pp_stmt s;
       Cil_datatype.Stmt.Hashtbl.remove stmt_init_table succ in



      ChangeDoChildrenPost (s,make_assign_block new_stmtkinds)

    with
      Not_found (* Stmt.Hashtbl.find stmt_init_table s *) -> DoChildren
    | Failure _ (*"hd"*) -> DoChildren

end
*)
