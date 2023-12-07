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

  let _dig1 x =
    String.filter ~f:Char.is_digit x
    |> String.to_list |> List.map ~f:Char.to_string

  let line_value f l =
    let dig = f l in
    let v = Int.of_string (List.hd_exn dig ^ List.last_exn dig) in
    Stdio.printf "%s: %d\n" l v; v


  let part1 (_input : input) : output = 69
  (* Fails on first example *)
  (* List.sum (module Int) ~f:(line_value dig1) input *)

  let dig2 x =
    let words =
      [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
      |> List.mapi ~f:(fun n name ->
             Angstrom.(string name |> map ~f:(fun _ -> Int.to_string (n + 1))))
    in
    let char_s =
      Angstrom.satisfy Char.is_digit |> Angstrom.map ~f:Char.to_string
    in
    let valid = Angstrom.choice (char_s :: words) in
    let all =
      Angstrom.(sep_by (advance 1) (many valid) |> map ~f:List.concat)
    in
    Angstrom.(parse_string ~consume:Consume.All all x) |> Result.ok_or_failwith

  let part2 (input : input) : output =
    List.sum (module Int) ~f:(line_value dig2) input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
