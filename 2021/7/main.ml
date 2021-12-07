open Base

let check_cost cf l c =
  List.map ~f:(fun p -> cf @@ Int.abs (p - c)) l |> List.reduce_exn ~f:(+)

let solve cf l =
  let min = Option.value_exn @@ List.min_elt ~compare:Int.compare l in
  let max = Option.value_exn @@ List.max_elt ~compare:Int.compare l in
  List.range ~start:`inclusive ~stop:`inclusive min max
  |> List.map ~f:(check_cost cf l)
  |> List.min_elt ~compare:Int.compare |> Option.value_exn

let solve1 l = solve Fn.id l
let solve2 l = solve (fun n -> n * (n + 1) / 2) l

let convert_data (l : string list) : int list =
  List.hd_exn l |> String.split ~on:',' |> List.map ~f:Int.of_string

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
