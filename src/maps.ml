
module Cl =
  Map.Make( struct
    type t = Model.id_name list
    let compare (x:t) (y:t) = compare x y
  end)


module Loc = Map.Make (
  struct
    type t = Ast.Loc.atom
    let compare (x:t) (y:t) = compare x y
  end
  )
