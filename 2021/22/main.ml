open Gg
open Base

module Cuboid = struct
  type t = {box: box3; kind: [`Full | `Negative]}

  let create box = {kind = `Full; box}

  let reverse x =
    {x with kind = match x.kind with `Full -> `Negative | _ -> `Full}

  let volume {box; kind} = match kind with
    | `Full -> Box3.volume box
    | `Negative -> -. (Box3.volume box)

  let intersections zone x =
    Sequence.of_list zone
    |> Sequence.map ~f:(fun c -> {c with box = Box3.inter c.box x})
    |> Sequence.filter ~f:(fun {box; _} -> not @@ Box3.is_empty box)

  let place zone x =
    (intersections zone x |> Sequence.map ~f:reverse |> Sequence.to_list) @ zone
end

let total_volume zone = List.sum (module Float) zone ~f:Cuboid.volume

let make_zone l = 
  List.fold l ~init:[] ~f:(fun zone (turn_on, box) ->
      let zone = Cuboid.place zone box in
      if turn_on then (Cuboid.create box) :: zone else zone
    )
  |> total_volume |> Float.round |> Int.of_float

let solve1 l =
  let init_cube = Box3.v (P3.v (-51.) (-51.) (-51.)) (Size3.v 102. 102. 102.) in
  List.filter l ~f:(fun (_, b) -> Box3.subset b init_cube) |> make_zone

let solve2 = make_zone

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
