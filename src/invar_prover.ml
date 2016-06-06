open Cil_types

let poly_of_term left_term right_term = 


let poly_of_pred (pred:predicate) = 
  match pred with
    Prel (_,tl,tr) -> 
      poly_of_term tl tr
  | _ -> assert false
  
(*
let invariant_to_poly (annot:code_annotation) = 
  
  match annot.annot_content with
    AInvariant (_,_,pred) -> pred_to_poly pred.content
  | _ -> None
*)
