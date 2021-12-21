open Base

module type S = sig
  type t
  include Comparable.S with type t := t
  include Sexpable.S with type t := t
end

(* just an experiment *)
module Make (A : S) (B : S) : S with type t = A.t * B.t = struct
  module O = struct
    type t = A.t * B.t
    let compare (a1,b1) (a2,b2) =
      let c = A.compare a1 a2 in if c = 0 then B.compare b1 b2 else c
    let sexp_of_t (a, b) = Sexp.List [A.sexp_of_t a; B.sexp_of_t b]
    let t_of_sexp = function 
      | Sexp.List [a; b] -> (A.t_of_sexp a, B.t_of_sexp b)
      | _ -> assert false
  end
  include O
  include Comparable.Make(O)
  include Comparator.Make(O)
end
