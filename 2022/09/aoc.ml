open Aoclib

module Types = struct
  type dir = Right | Up | Left | Down [@@deriving show]
  type move = { dir : dir; len : int } [@@deriving show]
  type input = move list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let dir =
    char 'R' *> return Right
    <|> char 'U' *> return Up
    <|> char 'L' *> return Left
    <|> char 'D' *> return Down

  let move =
    lift2
      (fun dir len -> { dir; len })
      (dir <* char ' ')
      (integer <* end_of_line)

  let input = many1 move
end

module Solving = struct
  open Base

  module Coord = struct
    module O = struct
      type t = { x : int; y : int }

      let compare = Poly.compare

      let sexp_of_t { x; y } =
        Sexp.(List [ Atom (Int.to_string x); Atom (Int.to_string y) ])
    end

    include O
    include Comparable.Make (O)
  end

  open Coord.O

  let flatten moves =
    List.concat_map moves ~f:(fun { dir; len } ->
        List.init len ~f:(Fn.const dir))

  let move c = function
    | Right -> { c with x = c.x + 1 }
    | Left -> { c with x = c.x - 1 }
    | Up -> { c with y = c.y + 1 }
    | Down -> { c with y = c.y - 1 }

  let step (head, tail) =
    let dx = head.x - tail.x in
    let dy = head.y - tail.y in
    let clamp1 = Int.clamp_exn ~min:(-1) ~max:1 in
    let tail =
      let adjusted = { x = tail.x + clamp1 dx; y = tail.y + clamp1 dy } in
      if abs dx >= 2 || abs dy >= 2 then adjusted else tail
    in
    tail

  let full_step dir = function
    | head :: rope ->
        let head = move head dir in
        let f prev cur =
          let after = step (prev, cur) in
          (after, after)
        in
        head :: List.folding_map rope ~init:head ~f
    | [] -> assert false

  let simulate_rope rope moves =
    let f (s, rope) dir =
      let rope' = full_step dir rope in
      (Set.add s (List.last_exn rope'), rope')
    in
    flatten moves |> List.fold ~init:(Set.empty (module Coord), rope) ~f

  let simulate_length moves length =
    let rope = List.init length ~f:(Fn.const { x = 0; y = 0 }) in
    let set, _ = simulate_rope rope moves in
    Set.length set

  let part1 (input : input) : output = simulate_length input 2
  let part2 (input : input) : output = simulate_length input 10
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
