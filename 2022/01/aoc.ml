type calory = int
[@@deriving show]

type elve = calory list
[@@deriving show]

type input = elve list
[@@deriving show]

type output = calory option
[@@deriving show]

module Parsing = struct
  open Angstrom
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let calory = integer

  let elve = sep_by end_of_line calory

  let input : input t = sep_by end_of_line elve

  let go str =
    match parse_string ~consume:All input str with
    | Ok v -> v
    | Error msg -> failwith msg
end

module Solving = struct
  open Base

  let part1 (input : input) : output =
    List.map ~f:(List.sum ~f:(Fn.id) (module Int)) input
    |> List.max_elt ~compare

  let part2 (input : input) : output =
    let sorted_sums =
      List.map ~f:(List.sum ~f:(Fn.id) (module Int)) input
      |> List.sort ~compare
      |> List.rev
    in
    List.sum (module Int) ~f:(Fn.id) (List.take sorted_sums 3)
    |> Option.some
end

let go file =
  Format.printf "%s@.%!" file;
  let input = Stdio.In_channel.read_all file |> Parsing.go in
  let o1 = Solving.part1 input in
  let o2 = Solving.part2 input in
  Format.printf "Part 1: %a@.%!" pp_output o1;
  Format.printf "Part 2: %a@.%!" pp_output o2

let _ =
  go "example";
  go "input"

