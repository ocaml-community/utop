
module Tailrec_List = struct
  include List

  let map f l =
    List.rev (List.rev_map f l)

  let map2 f l l' =
    List.rev (List.rev_map2 f l l')

  let concat l =
    let rec aux acc = function
    | [] -> acc
    | h :: t ->
       aux (List.rev_append h acc) t
    in
    List.rev (aux [] l)

end

module StdLabels = struct
  module Array = StdLabels.Array
  module Bytes = StdLabels.Bytes
  module List = struct
    include StdLabels.List

    let map ~f l = Tailrec_List.map f l
    let concat = Tailrec_List.concat
  end
  module String = StdLabels.String
end

module List = Tailrec_List
