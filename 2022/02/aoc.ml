open Aoclib

module Types = struct
  type play = Rock | Paper | Scissors [@@deriving show]
  type round = { me : play; them : play } [@@deriving show]
  type input = round list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom

  let convert = function
    | 'X' | 'A' -> Some Rock
    | 'Y' | 'B' -> Some Paper
    | 'Z' | 'C' -> Some Scissors
    | _ -> None

  let play =
    satisfy (fun c -> Option.is_some (convert c)) >>| fun c ->
    Option.get (convert c)

  let round =
    play <* char ' ' >>= fun them ->
    play <* end_of_line >>| fun me -> { me; them }

  let input = many round
end

module Solving = struct
  open Base

  let outcome r =
    match (r.me, r.them) with
    | a, b when Poly.(a = b) -> `Draw
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> `Win
    | _ -> `Lose

  let score_round r =
    let out_score =
      match outcome r with `Lose -> 0 | `Draw -> 3 | `Win -> 6
    in
    let shape_score =
      match r.me with Rock -> 1 | Paper -> 2 | Scissors -> 3
    in
    out_score + shape_score

  let part1 (input : input) : output =
    List.sum (module Int) input ~f:score_round

  let reinterpret r =
    let me =
      match (r.them, r.me) with
      | a, Paper -> a (* draw *)
      | Rock, Rock -> Scissors (* lose *)
      | Paper, Rock -> Rock
      | Scissors, Rock -> Paper
      | Rock, Scissors -> Paper (* win *)
      | Paper, Scissors -> Scissors
      | Scissors, Scissors -> Rock
    in
    score_round { r with me }

  let part2 (input : input) : output =
    List.sum (module Int) input ~f:reinterpret
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
