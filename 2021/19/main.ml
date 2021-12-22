open Gg
open Base
module Format = Caml.Format

(* Big ugly bruteforce *)

let equal_f_eps x y =
  let eps = 0.00001 in
  Float.(abs (x - y) < eps)

let compare_f_eps x y =
  if equal_f_eps x y then 0
  else Float.compare x y

let all_transformations =
  let r90 = Base.Float.pi /. 2. in
  let rotations =
    [0.; 1.; 2.; 3.]
    |> List.map ~f:(( *. ) r90)
  in
  List.cartesian_product (List.cartesian_product rotations rotations) rotations
  |> List.map ~f:(fun ((x, y), z) ->
      M4.rot3_zyx (V3.v x y z)
    )
  |> List.dedup_and_sort ~compare:(M4.compare_f compare_f_eps)

let trans_vecs t = List.map ~f:(fun vec -> V3.tr t vec)

let all_transformations s =
  all_transformations |> List.map ~f:(fun m -> trans_vecs m s)

let all_translations area scanner =
  List.cartesian_product area scanner
  |> List.map ~f:(fun (dest, orig) ->
      let delta = V3.sub dest orig in
      (List.map scanner ~f:(fun p -> V3.add p delta),
       delta)
    )

let is_detectable v1 v2 =
  V3.for_all (fun x -> Float.(abs x <= 1000.0001)) (V3.sub v1 v2)

let matches area (candidate, origin) =
  let detectable = List.filter area ~f:(is_detectable origin) in
  if List.length detectable < 12 then false
  else List.for_all detectable
      ~f:(List.mem ~equal:(V3.equal_f equal_f_eps) candidate)

let find_match area scanner =
  all_transformations scanner
  |> List.map ~f:(all_translations area)
  |> List.concat
  |> List.find ~f:(matches area)

let merge area candidate =
  List.concat [area; candidate]
  |> List.dedup_and_sort ~compare:(V3.compare_f compare_f_eps)

let rec add_to_area area origs = function
  | [] -> (area, origs)
  | scanner :: t ->
    match find_match area scanner with
    | None -> add_to_area area origs (t @ [scanner])
    | Some (s, o) -> add_to_area (merge area s) (o :: origs) t

let make_area l = match l with
  | h :: t -> add_to_area h [V3.v 0. 0. 0.] t
  | _ -> assert false

let solve1 l = let (area, _) = make_area l in List.length area

let solve2 l =
  let (_, origs) = make_area l in
  List.cartesian_product origs origs
  |> List.map ~f:(fun (x, y) ->
      V3.sub x y |> V3.map Float.abs |> V3.fold Float.add 0.
    )
  |> List.max_elt ~compare:Float.compare
  |> Option.value_exn
  |> Float.round_nearest |> Float.to_int

let convert_data (l : string list) : v3 list list =
  let parse_point s =
    let c = Float.of_int in
    Caml.Scanf.sscanf s "%d,%d,%d" (fun x y z -> V3.v (c x) (c y) (c z))
  in
  let parse_scanner l =
    List.drop_last_exn (List.drop l 1)
    |> List.map ~f:parse_point
  in
  List.group l ~break:(fun x _ -> String.is_empty x)
  |> List.map ~f:parse_scanner

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example"; main "input"
