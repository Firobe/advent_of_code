open Base

let coord_in_range a (x, y) =
  x >= 0 && y >= 0 && x < (Array.length a.(0))
  && y < (Array.length a)

let get_neighbor_coords a (x, y) =
  let deltas = [(1, 0); (0, 1); (-1, 0); (0, -1);
                (1, 1); (-1, -1); (1, -1); (-1, 1)] in
  let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
  List.filter ~f:(coord_in_range a) coords

let all_coords a =
  let xs = List.range 0 (Array.length a.(0)) in
  let ys = List.range 0 (Array.length a) in
  List.cartesian_product xs ys

let map_array2 a ~f = Array.mapi a ~f:(fun y a' ->
    Array.mapi a' ~f:(fun x v -> f (x, y) v))

let flash_quantum l =
  let increase (x, y) =
    get_neighbor_coords l (x, y)
    |> List.count ~f:(fun (x, y) -> Poly.(l.(y).(x) = `Flashing))
  in
  map_array2 l ~f:(fun (x, y) -> function
      | `Val v -> `Val (v + increase (x, y))
      | a -> a)
  |> map_array2 ~f:(fun _ -> function
      | `Flashing -> `Flashed
      | a -> a)

let step (l, sum) = 
  let complete l = 
    Array.for_all l ~f:(Array.for_all ~f:(fun x -> Poly.(x <> `Flashing)))
  in let replace = map_array2 ~f:(fun _ ->
      function `Val x -> if x > 9 then `Flashing else `Val x | a -> a)
  in let l = map_array2 l ~f:(fun _ ->
      function `Val x -> `Val (x + 1) | a -> a)
  in
  let rec aux l =
    let l = replace l in
    if complete l then l else aux (flash_quantum l)
  in
  let result = aux l in
  let flashes = Array.sum (module Int) result
      ~f:(Array.count ~f:(Poly.((=) `Flashed))) in
  let final = map_array2 result ~f:(fun _ -> function `Flashed -> `Val 0 | a -> a) in
  (final, flashes + sum)

let solve1 l = Fn.apply_n_times ~n:100 step (l, 0) |> snd

let solve2 l =
  let goal = (Array.length l * (Array.length l.(0))) in
  let rec aux n l =
    let l', sum' = step (l, 0) in
    if sum' = goal then n
    else aux (n + 1) l'
  in aux 1 l

let convert_data (l : string list) =
  Array.of_list_map ~f:(fun l ->
      String.to_list l |> Array.of_list_map ~f:(Fn.compose Int.of_string String.of_char)
      |> Array.map ~f:(fun x -> `Val x)
    ) l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
