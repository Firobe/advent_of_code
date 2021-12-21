open Base

module Coord = Pair.Make(Int)(Int)

(* if negative is true, then the image represents reversed pixels *)
type image = {negative: bool; d: Set.M(Coord).t}

let image_boundaries (i : image) : Coord.t * Coord.t =
  let xmax, _ = Set.max_elt_exn i.d in
  let xmin, _ = Set.min_elt_exn i.d in
  let ymin, ymax = Set.fold i.d ~init:(Int.max_value, Int.min_value)
      ~f:(fun (ymin, ymax) (_, y) -> (Int.min ymin y, Int.max ymax y)) in
  (xmin - 1, xmax + 1), (ymin - 1, ymax + 1)

let get_neighbor_coords (x, y) : Coord.t list =
  [(-1, -1); (0, -1) ; (1, -1) ;
   (-1, 0) ; (0, 0)  ; (1, 0)  ;
   (-1, 1) ; (0, 1)  ; (1, 1)]
  |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))

let get_index (i : image) (p : Coord.t) : int =
  get_neighbor_coords p
  |> List.fold ~init:0 ~f:(fun sum p ->
      (sum lsl 1) lor (if Bool.(Set.mem i.d p <> i.negative) then 1 else 0)
    )

let step (a : bool array) (i : image) : image =
  let negative = Bool.(a.(0) <> i.negative) in
  let (xmin, xmax), (ymin, ymax) = image_boundaries i in
  let d = List.cartesian_product
    (List.range ~stop:`inclusive xmin xmax)
    (List.range ~stop:`inclusive ymin ymax)
  |> List.fold ~init:(Set.empty (module Coord)) ~f:(fun set p ->
      if Bool.(a.(get_index i p) <> negative) then Set.add set p else set
    )
  in {negative; d}

let solve1 (a, i) = (Fn.apply_n_times ~n:2 (step a) i).d |> Set.length
let solve2 (a, i) = (Fn.apply_n_times ~n:50 (step a) i).d |> Set.length

let convert_data (l : string list) : bool array * image = match l with
  | algo :: "" :: image ->
    let algo = String.to_array algo |> Array.map ~f:(Char.equal '#') in
    let set = List.foldi image ~init:(Set.empty (module Coord)) ~f:(fun y set row ->
        String.foldi row ~init:set ~f:(fun x set c ->
            if Char.(c = '#') then Set.add set (x, y) else set
          )
      )
    in algo, {negative = false; d = set}
  | _ -> failwith "Wrong input"

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
