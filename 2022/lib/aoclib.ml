module type Types = sig
  type input
  type output
  val pp_output : Format.formatter -> output -> unit
end
module type Parsing = sig
  type input
  val input : input Angstrom.t
end

module type Solving = sig
  type input
  type output
  val part1 : input -> output
  val part2 : input -> output
end

module Parsing = struct
  open Angstrom
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
end

module MakeDay
    (T : Types)
    (P : Parsing with type input := T.input)
    (S : Solving with type input := T.input and type output := T.output) =
struct

  let go file =
    Format.printf "%s@.%!" file;
    let do_parse str = 
      let open Angstrom in
      match parse_string ~consume:All P.input str with
      | Ok v -> v
      | Error msg -> failwith msg
    in
    let input = Stdio.In_channel.read_all file |> do_parse in
    let o1 = S.part1 input in
    let o2 = S.part2 input in
    Format.printf "Part 1: %a@.%!" T.pp_output o1;
    Format.printf "Part 2: %a@.%!" T.pp_output o2

  let run_all = go "example"; go "input"
end
