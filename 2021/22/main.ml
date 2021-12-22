open Base
open Gg

type cuboid = {box: box3; kind: [`Full | `Negative]}

let intersections zone x =
  Sequence.of_list zone
  |> Sequence.map ~f:(fun c -> {c with box = Box3.inter c.box x})
  |> Sequence.filter ~f:(fun {box; _} -> not @@ Box3.is_empty box)

let add_cuboid (zone, volume) added =
  let volume', to_add =
    intersections zone added
    |> Sequence.fold ~init:(volume +. Box3.volume added, [])
      ~f:(fun (volume, nc) inter -> match inter.kind with
          | `Full ->
            (volume -. Box3.volume inter.box, {kind = `Negative; box = inter.box} :: nc)
          | `Negative ->
            (volume +. Box3.volume inter.box, {kind = `Full; box = inter.box} :: nc)
        )
  in ({kind = `Full; box = added} :: to_add @ zone, volume')

let remove_cuboid (zone, volume) removed =
  let volume', to_add =
    intersections zone removed 
    |> Sequence.fold ~init:(volume, [])
      ~f:(fun (volume, nc) inter -> match inter.kind with
          | `Full ->
            (volume -. Box3.volume inter.box, {kind = `Negative; box = inter.box} :: nc)
          | `Negative ->
            (volume +. Box3.volume inter.box, {kind = `Full; box = inter.box} :: nc)
        )
  in (to_add @ zone, volume')

let total_volume l = 
  let (_, v) = List.fold l ~init:([], 0.) ~f:(fun (zone, volume) (k, b) ->
      let f = if k then add_cuboid else remove_cuboid in
      f (zone, volume) b
    )
  in v |> Float.round |> Int.of_float

let solve1 l =
  let init_cube = Box3.v (P3.v (-51.) (-51.) (-51.)) (Size3.v 102. 102. 102.) in
  List.filter l ~f:(fun (_, b) -> Box3.subset b init_cube)
  |> total_volume

let solve2 = total_volume

let convert_data (l : string list) =
  let c = Int.to_float in
  List.map l ~f:(fun s ->
      Caml.Scanf.sscanf s "%s x=%d..%d,y=%d..%d,z=%d..%d"
        (fun kind xmin xmax ymin ymax zmin zmax ->
           let p1 = P3.v (c xmin -. 0.5) (c ymin -. 0.5) (c zmin -. 0.5) in
           let p2 = P3.v (c xmax +. 0.5) (c ymax +. 0.5) (c zmax +. 0.5) in
           Poly.(kind = "on", Box3.of_pts p1 p2)
        )
    )

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "small"; main "example" ; main "example2" ; main "input"
