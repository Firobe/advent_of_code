open Aoclib

module Types = struct
  type color = [ `Red | `Green | `Blue ] [@@deriving show]
  type round' = (int * color) list [@@deriving show]
  type round = { r : int; g : int; b : int } [@@deriving show]
  type game = round list [@@deriving show]
  type input = game list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let str_v s v = string s *> return v

  let color =
    choice [ str_v "red" `Red; str_v "green" `Green; str_v "blue" `Blue ]

  let set = both (integer <* string " ") color
  let round' = sep_by1 (string ", ") set

  let round =
    let aux set = function
      | n, `Red -> { set with r = n }
      | n, `Green -> { set with g = n }
      | n, `Blue -> { set with b = n }
    in
    round' >>| List.fold_left aux { r = 0; g = 0; b = 0 }

  let game = sep_by1 (string "; ") round

  let input =
    many (string "Game " *> integer *> string ": " *> game <* end_of_line)
end

module Solving = struct
  open Base

  let possible r' g' b' =
    List.for_all ~f:(fun { r; g; b } -> r <= r' && g <= g' && b <= b')

  let part1 (input : input) : output =
    List.filter_mapi input ~f:(fun n game ->
        if possible 12 13 14 game then Some (n + 1) else None)
    |> List.sum (module Int) ~f:Fn.id

  let union s1 s2 = { r = max s1.r s2.r; g = max s1.g s2.g; b = max s1.b s2.b }

  let power game =
    let { r; g; b } = List.reduce_exn ~f:union game in
    r * g * b

  let part2 (input : input) : output = List.sum (module Int) input ~f:power
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
