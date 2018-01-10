let (<<) f g x = f (g x)
let (>>) f g x = g (f x)


module IntT = struct
  type t = int
  let compare = compare
end

module StringT = struct
  type t = string
  let compare = compare
end

module IntMap = Map.Make(IntT)
module StringMap = Map.Make(StringT)

module MapLens(Key : Map.OrderedType) = struct
  let for_key key =
    let module M = Map.Make(Key) in
    let open Lens in
    { get = M.find key
    ; set = M.add key
    }
end

module IntMapLens = MapLens(IntT)
module StringMapLens = MapLens(StringT)


let button' msg description =
  let open Tea.Html in
  button [ onClick msg ] [ text description ]
