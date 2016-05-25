type vec = Pilat_matrix.QMat.vec 
type mat = Lacaml_D.mat 


(** An invariant is an eigenspace, represented by its base with
    a vec list. 
    When an eigenspace is associated to an eigenvalue strictly 
    lower to one, the invariant is convergent 
    (<e,X> < k => <e,MX> <k).
    When it is higher to one, it is divergent
    (<e,X> > k => <e,MX> > k).
*)

type limit = 
  Convergent 
| Divergent 
| Altern
| One
| Zero

type invar = limit * vec list


val lim_to_string : limit -> string


(** Returns the rational eigenspaces union of the floating matrix 
    as a list of bases. *)
val invariant_computation : mat -> invar list

(** Intersects two union of vectorial spaces. *)
val intersection_invariants :  invar list -> invar list -> invar list

(** After the integration, there is no fraction left on the vector expression. *)
val integrate_vec : vec -> vec
