open Base

type coord = {x : int; y : int}
type line = {source : coord; dest : coord; delta : coord}

let iter_line ~(f : coord -> unit) (l : line) =
  let rec aux (cur : coord) =
    if Poly.(cur = l.dest) then f cur
    else (
      f cur; aux {x = cur.x + l.delta.x; y = cur.y + l.delta.y}
    )
  in aux l.source

let solve ~all_lines l =
  let m = Array.make_matrix ~dimx:1000 ~dimy:1000 0 in
  List.iter l ~f:(fun l ->
      if all_lines || l.source.x = l.dest.x || l.source.y = l.dest.y then
        iter_line l ~f:(fun {x; y} -> m.(x).(y) <- 1 + m.(x).(y))
    );
  Array.map m ~f:(Array.count ~f:(fun x -> x > 1)) |> Array.reduce_exn ~f:(+)

let solve1 l = solve ~all_lines:false l
let solve2 l = solve ~all_lines:true l

let convert_data (l : string list) : line list =
  let sign_delta i = match Int.sign i with Neg -> -1 | Zero -> 0 | Pos -> 1 in
  let to_coord s = match String.split ~on:',' s with
    | x :: [y] -> {x = Int.of_string x; y = Int.of_string y}
    | _ -> assert false
  in let to_line s = match String.split ~on:' ' s with
      | l1 :: _ :: [l2] ->
        let source = to_coord l1 in
        let dest = to_coord l2 in
        let delta = {x = sign_delta (dest.x - source.x);
                     y = sign_delta (dest.y - source.y)} in
        {source ; dest; delta}
      | _ -> assert false
  in List.map ~f:to_line l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file (solve1 input) (solve2 input)

let () = main "example" ; main "input"
