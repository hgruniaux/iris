type t = { id : int; name : string option }

let fresh =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    { id = !cpt; name = None }

(* FIXME: remove this *)
let fresh_with_id ?(name = None) id = { id; name }
let compare (x : t) (y : t) = Stdlib.compare x.id y.id
