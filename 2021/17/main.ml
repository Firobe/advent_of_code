open Base

let y_at ~v_0 k = (k + 1) * (k - 2 * v_0) / -2
let x_at ~v_0 k = if k > v_0 then y_at ~v_0 v_0 else y_at ~v_0 k
let check_range ~min ~max v =
  if v < min then `Under else if v > max then `Over else `Inside

let find_inside_steps ~f ~min ~max v_0 =
  let rec aux k =
    match check_range ~min ~max (f ~v_0 k) with
    | `Under when k > v_0 -> []
    | `Inside -> k :: aux (k + 1)
    | _ -> aux (k + 1)
  in aux 0

let x_is_possible ~valid_steps (x1, x2) x =
  List.exists valid_steps ~f:(fun k ->
      Poly.(check_range ~min:x1 ~max:x2 (x_at ~v_0:x k) = `Inside)
    )

let y_is_possible (x1, x2) (y1, y2) y =
  let valid_steps = find_inside_steps ~f:y_at ~min:y1 ~max:y2 y in
  if List.is_empty valid_steps then []
  else(
    List.range 0 x2
    |> List.filter ~f:(x_is_possible ~valid_steps (x1, x2))
  )

let find_all xrange yrange =
  List.range (-1000) 1000
  |> List.map ~f:(fun y ->
      List.map (y_is_possible xrange yrange y) ~f:(fun x -> (x, y)))
  |> List.concat
  
let solve1 (xrange, yrange) =
  let all = find_all xrange yrange in
  List.iter ~f:(fun (x, y) -> Stdio.printf "(%d,%d) " x y) all;
  let (_, v_0) =
    all |> List.max_elt ~compare:(fun (_,a) (_,b) -> compare a b)
    |> Option.value_exn
  in y_at ~v_0 v_0

let solve2 (xrange, yrange) =
  find_all xrange yrange |> List.length

let convert_data (l : string list) : (int * int) * (int * int) =
  let s = List.hd_exn l in
  Caml.Scanf.sscanf s "target area: x=%d..%d, y=%d..%d"
    (fun a b c d -> (a, b), (c, d))

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
