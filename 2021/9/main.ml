open Base

let coord_in_range a (x, y) =
  x >= 0 && y >= 0 && x < (Array.length a.(0))
  && y < (Array.length a)

let get_neighbor_coords a (x, y) =
  let deltas = [(1, 0); (0, 1); (-1, 0); (0, -1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(coord_in_range a) coords

let is_low_point a (x, y) =
  let get_coord a (x, y) = a.(y).(x) in
  let neighbors = get_neighbor_coords a (x, y) |> List.map ~f:(get_coord a) in
  List.for_all neighbors ~f:(fun v -> v > a.(y).(x))

let all_coords a =
  let xs = List.range 0 (Array.length a.(0)) in
  let ys = List.range 0 (Array.length a) in
  List.cartesian_product xs ys

let low_points l =
  all_coords l |> List.filter_map ~f:(fun (x, y) ->
      if is_low_point l (x, y) then Some (x, y) else None
    )

let solve1 l = low_points l |> List.sum (module Int) ~f:(fun (x, y) -> l.(y).(x) + 1)

let map_array2 a ~f = Array.mapi a ~f:(fun y a' ->
    Array.mapi a' ~f:(fun x v -> f (x, y) v))

let trickle_step l mask =
  let trickle_to (x, y) =
    get_neighbor_coords l (x, y)
    |> List.find_map ~f:(fun (x, y) ->
        match mask.(y).(x) with
        | `Basin id -> Some (`Basin id)
        | _ -> None)
    |> Option.value ~default:`Unknown
  in
  map_array2 mask ~f:(fun (x, y) -> function
      | `Unknown -> trickle_to (x, y)
      | a -> a
    )

let full_trickle l mask =
  let complete mask =
    Array.for_all mask ~f:(Array.for_all ~f:(fun x -> Poly.(x <> `Unknown)))
  in
  let rec aux mask =
    if complete mask then mask
    else aux (trickle_step l mask)
  in aux mask

let solve2 l =
  let lp = low_points l in
  let lp_ids = List.range 0 (List.length lp) |> List.zip_exn lp in
  let mask = map_array2 l ~f:(fun (x, y) v ->
      match Caml.ListLabels.assoc_opt (x, y) lp_ids with
      | Some id -> `Basin id
      | None -> if v = 9 then `Stop else `Unknown
    )
  in
  let after_trickle = full_trickle l mask in
  let basins =
    all_coords l
    |> List.filter_map ~f:(fun (x, y) -> match after_trickle.(y).(x) with `Basin id -> Some id | _ -> None)
  in
  let freqs = List.range 0 (List.length lp)
  |> List.map ~f:(fun bid -> bid, List.count basins ~f:((=) bid))
  |> List.sort ~compare:(fun (_, s1) (_, s2) -> compare s2 s1) in
  List.take freqs 3 |> List.map ~f:snd |> List.reduce_balanced_exn ~f:( * )

let convert_data (l : string list) : int array array =
  Array.of_list_map ~f:(fun l ->
      String.to_list l |> Array.of_list_map ~f:(Fn.compose Int.of_string String.of_char)
    ) l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
