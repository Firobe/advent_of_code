
let solve1 l =
  let apply_delta (x, d) (dir, delta) = match dir with
    | `Forward -> (x + delta, d)
    | `Down -> (x, d + delta)
    | `Up -> (x, d - delta)
  in let (x, d) = List.fold_left apply_delta (0, 0) l in x * d

let solve2 l =
  let apply_delta (x, d, a) (dir, delta) = match dir with
    | `Forward -> (x + delta, d + a * delta, a)
    | `Down -> (x, d, a + delta)
    | `Up -> (x, d, a - delta)
  in let (x, d, _) = List.fold_left apply_delta (0, 0, 0) l in x * d

let convert_line s =
  Scanf.sscanf s "%s %d" (fun dir x ->
      let dir = match dir with
        | "forward" -> `Forward
        | "down" -> `Down
        | "up" -> `Up
        | _ -> failwith "Wrong input"
      in (dir, x))

let main file =
  let input = Arg.read_arg file |> Array.to_list |> List.map convert_line in
  Printf.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file (solve1 input) (solve2 input)

let () =
  main "example" ; main "input"
