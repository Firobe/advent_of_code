open Aoclib

module Types = struct
  type path = (int * int) list [@@deriving show]
  type input = path list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let path =
    sep_by (string " -> ") (both integer (char ',' *> integer)) <* end_of_line

  let input = many path
end

module Solving = struct
  open Base

  type cell = Void | Block | Sand

  let make_terrain paths =
    let a = Array.make_matrix ~dimx:1000 ~dimy:1000 Void in
    let rec fill_seg (x1, y1) (x2, y2) =
      a.(x1).(y1) <- Block;
      if x1 = x2 && y1 = y2 then ()
      else if x1 = x2 then
        let dy = y2 - y1 in
        fill_seg (x1, y1 + (dy / abs dy)) (x2, y2)
      else
        let dx = x2 - x1 in
        fill_seg (x1 + (dx / abs dx), y1) (x2, y2)
    in
    let rec fill_path = function
      | start :: stop :: t ->
          fill_seg start stop;
          fill_path (stop :: t)
      | _ -> ()
    in
    List.iter paths ~f:fill_path;
    a

  let next_sand ~floor t =
    let open Poly in
    let rec aux (x, y) =
      if t.(x).(y) = Sand then None
      else if y = 999 then None
      else if y + 1 = floor then Some (x, y)
      else if t.(x).(y + 1) = Void then aux (x, y + 1)
      else if x = 0 || t.(x - 1).(y + 1) = Void then aux (x - 1, y + 1)
      else if x = 999 || t.(x + 1).(y + 1) = Void then aux (x + 1, y + 1)
      else Some (x, y)
    in
    aux (500, 0)

  let simulate_max ~floor t =
    let rec aux n =
      match next_sand ~floor t with
      | None -> n
      | Some (x, y) ->
          t.(x).(y) <- Sand;
          aux (n + 1)
    in
    aux 0

  let part1 (input : input) : output =
    make_terrain input |> simulate_max ~floor:1000

  let part2 (input : input) : output =
    let floor =
      (List.concat_map input ~f:(List.map ~f:snd)
      |> List.max_elt ~compare |> Option.value_exn)
      + 2
    in
    make_terrain input |> simulate_max ~floor
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
