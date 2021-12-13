open Base

type input = {coords : (int * int) list; folds : ([`Up | `Left] * int) list}

let fold coords (dir, v) =
  let f ((x, y) as p) = match dir with
    | `Up -> if y > v then (x, v - (y - v - 1) - 1) else p
    | `Left -> if x > v then (v - (x - v - 1) - 1, y) else p

  in List.map ~f coords |> List.dedup_and_sort ~compare:Poly.compare

let print_paper coords =
  let xs, ys = List.unzip coords in
  let max_x = List.max_elt xs ~compare |> Option.value_exn in
  let max_y = List.max_elt ys ~compare |> Option.value_exn in
  for y = 0 to max_y do
    for x = 0 to max_x do
      if List.exists coords ~f:(Poly.equal (x, y)) then
        Stdio.printf "#"
      else Stdio.printf " "
    done;
    Stdio.printf "\n"
  done

let solve1 t = fold t.coords (List.hd_exn t.folds) |> List.length
let solve2 t = List.fold ~f:fold ~init:t.coords t.folds |> print_paper

let convert_data (l : string list) =
  let coords, folds = List.split_while l ~f:(Fn.non String.is_empty) in
  let coords = List.map coords ~f:(fun s -> match String.split ~on:',' s with
      | [x; y] -> (Int.of_string x, Int.of_string y)
      | _ -> failwith "Wrong input")
  in
  let folds = List.tl_exn folds |> List.map ~f:(fun s ->
      Caml.Scanf.sscanf s "fold along %c=%d" (fun s v ->
          ((if Char.(s = 'x') then `Left else `Up), v)))
  in {coords; folds}

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 SEE BELOW\n" file
    (solve1 input);
  solve2 input

let () = main "example" ; main "input"
