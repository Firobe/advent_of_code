open Aoclib

module Types = struct
  type input = int list list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom

  (*open Parsing*)
  let digit =
    satisfy Base.Char.is_digit >>| fun x -> Base.Char.(to_int x - to_int '0')

  let line = many digit <* end_of_line
  let input = many line
end

module Solving = struct
  open Base

  let merge_lines ~mul a b = List.map2_exn ~f:mul a b

  let process_line ~f ~mul l =
    merge_lines ~mul (f l) (f (List.rev l) |> List.rev)

  let process_grid_lines ~f ~mul = List.map ~f:(process_line ~f ~mul)

  let process_grid ~f ~mul g =
    List.map2_exn ~f:(merge_lines ~mul)
      (process_grid_lines ~f ~mul g)
      (process_grid_lines ~f ~mul (List.transpose_exn g) |> List.transpose_exn)

  let visible_lr l =
    let f curmax v = (max curmax v, v > curmax) in
    List.folding_map l ~init:0 ~f

  let part1 (input : input) : output =
    process_grid ~f:visible_lr ~mul:( || ) input
    |> List.concat |> List.count ~f:Fn.id

  let scenic_lr l =
    let max = Option.merge ~f:max in
    let max_above n a =
      Array.sub a ~pos:n ~len:(10 - n) |> Array.reduce_exn ~f:max
    in
    let f (last_seen, i) v =
      let s = i - Option.value ~default:0 (max_above v last_seen) in
      last_seen.(v) <- Some i;
      ((last_seen, i + 1), s)
    in
    List.folding_map l ~init:(Array.create ~len:10 None, 0) ~f

  let part2 (input : input) : output =
    let g = process_grid ~f:scenic_lr ~mul:( * ) input in
    List.concat g |> List.max_elt ~compare |> Option.value_exn
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
