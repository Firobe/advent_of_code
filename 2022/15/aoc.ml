open Aoclib

module Types = struct
  type coord = { x : int; y : int } [@@deriving show { with_path = false }]

  type line = { sensor : coord; closest : coord }
  [@@deriving show { with_path = false }]

  type input = line list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let coord =
    lift2
      (fun x y -> { x; y })
      (string "x=" *> neg_integer)
      (string ", y=" *> neg_integer)

  let line =
    lift2
      (fun sensor closest -> { sensor; closest })
      (string "Sensor at " *> coord)
      (string ": closest beacon is at " *> coord)
    <* end_of_line

  let input = many1 line
end

module Solving = struct
  open Base
  module D = Diet.Make (Stdlib.Int)

  let dist c1 c2 = abs (c2.x - c1.x) + abs (c2.y - c1.y)
  let effective_range s = dist s.sensor s.closest

  let intersect_range ~y s =
    let range = effective_range s in
    let vert_dist = dist s.sensor { s.sensor with y } in
    let delta = range - vert_dist in
    let set = D.empty in
    if 0 <= delta then
      D.add (D.Interval.make (s.sensor.x - delta) (s.sensor.x + delta)) set
    else set

  let compute_interval_set ~y l =
    List.map l ~f:(intersect_range ~y) |> List.reduce_exn ~f:D.union

  let remove_known ~y l candidates =
    List.fold l ~init:candidates ~f:(fun s { closest; _ } ->
        if closest.y = y && D.mem closest.x s then
          D.remove (D.Interval.make closest.x closest.x) s
        else s)

  let part1 (input : input) : output =
    let y = if List.length input > 14 then 2000000 else 10 in
    compute_interval_set ~y input |> remove_known ~y input |> D.cardinal

  let restrict base s = D.fold (fun i s -> D.remove i s) s base

  let part2 (input : input) : output =
    let max = if List.length input > 14 then 4000000 else 20 in
    let possible_set = D.add (D.Interval.make 0 max) D.empty in
    let y, s =
      List.range 0 (max + 1)
      |> List.map ~f:(fun y -> compute_interval_set ~y input)
      |> List.map ~f:(restrict possible_set)
      |> List.findi_exn ~f:(fun _ s -> D.cardinal s > 0)
    in
    let x = D.choose s |> D.Interval.x in
    (x * 4000000) + y
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
