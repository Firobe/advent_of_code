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

  let part1 (_input : input) : output = 42
  (* Fails on second example, so removed part 1 *)

  let names =
    [
      "zero";
      "one";
      "two";
      "three";
      "four";
      "five";
      "six";
      "seven";
      "eight";
      "nine";
    ]

  let dig2 x =
    let open Angstrom in
    let char_s = satisfy Char.is_digit |> map ~f:Char.to_string in
    let scan f =
      choice
        (char_s
        :: List.mapi names ~f:(fun n name ->
               string (f name) |> map ~f:(fun _ -> Int.to_string n)))
    in
    let forward = scan Fn.id in
    let backward = scan String.rev in
    let all valid = sep_by (advance 1) (many valid) |> map ~f:List.concat in
    let parse f s =
      parse_string ~consume:Consume.All (all f) (s x)
      |> Result.ok_or_failwith |> List.hd_exn
    in
    let first = parse forward Fn.id in
    let last = parse backward String.rev in
    Int.of_string (first ^ last)

  let part2 (input : input) : output = List.sum (module Int) ~f:dig2 input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
