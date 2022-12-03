open Aoclib

module Types = struct
  type input = int list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = many (integer <* end_of_line)
end

module Solving = struct
  open Base

  let part1 (input : input) : output =
    let a, b =
      List.cartesian_product input input
      |> List.find_exn ~f:(fun (a, b) -> a + b = 2020)
    in
    a * b

  let part2 (input : input) : output =
    let a, (b, c) =
      List.cartesian_product input input
      |> List.cartesian_product input
      |> List.find_exn ~f:(fun (a, (b, c)) -> a + b + c = 2020)
    in
    a * b * c
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
