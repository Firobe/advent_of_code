open Aoclib

module Types = struct
  type range = {min: int; max: int} [@@deriving show]
  type pair = (range * range) [@@deriving show]
  type input = pair list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let range =
    lift2 (fun min max -> {min; max})
      integer
      (char '-' *> integer)

  let pair = lift2 (fun a b -> (a, b)) range (char ',' *> range)

  let input = many (pair <* end_of_line)
end

module Solving = struct
  open Base

  let contains inner outer =
    inner.min >= outer.min && inner.max <= outer.max

  let part1 (input : input) : output =
    let detect (a, b) = contains a b || contains b a in
    List.count ~f:detect input

  let overlap (a, b) =
    let aux left right = right.min <= left.max in
    if a.min < b.min then aux a b else aux b a

  let part2 (input : input) : output =
    List.count ~f:overlap input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
