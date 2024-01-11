type t = int

let fresh =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    !cpt

let compare (x : t) (y : t) = Stdlib.compare x y

module Map = Map.Make (Int)
module Set = Set.Make (Int)

type 'a map = 'a Map.t
type set = Set.t
