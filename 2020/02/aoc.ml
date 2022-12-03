open Aoclib

module Types = struct
  type policy = { letter : char; min : int; max : int } [@@deriving show]
  type input = (policy * string) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input =
    lift4
      (fun min max letter p -> ({ letter; min; max }, p))
      integer
      (char '-' *> integer)
      (char ' ' *> any_char <* string ": ")
      (take_while1 Base.Char.is_lowercase)
    <* end_of_line |> many
end

module Solving = struct
  open Base

  let check1 (policy, password) =
    let c = String.count password ~f:(Char.equal policy.letter) in
    policy.min <= c && c <= policy.max

  let check2 (policy, password) =
    let open Poly in
    password.[policy.min - 1]
    = policy.letter
    <> (password.[policy.max - 1] = policy.letter)

  let part1 (input : input) : output = List.count input ~f:check1
  let part2 (input : input) : output = List.count input ~f:check2
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
