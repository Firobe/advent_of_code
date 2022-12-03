open Aoclib

module Types = struct
  type input = string list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  (*open Parsing*)

  let input = many (consumed (many_till any_char end_of_line))
end

module Solving = struct
  open Base

  let part1 (input : input) : output = List.length input
  let part2 (input : input) : output = List.length input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
