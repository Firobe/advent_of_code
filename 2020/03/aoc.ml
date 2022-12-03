open Aoclib

module Types = struct
  let pp_array pp_elem fmt t =
    Array.to_seq t |> Format.(pp_print_seq pp_elem fmt)

  type input = bool array array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Base

  let bool = Fn.const false <$> char '.' <|> (Fn.const true <$> char '#')

  let input =
    many (many_till bool end_of_line >>| Array.of_list) >>| Array.of_list
end

module Solving = struct
  open Base

  let slope_coords ~down ~right max_down =
    Sequence.unfold ~init:(0, 0) ~f:(fun (x, y) ->
        if y > max_down then None else Some ((x, y), (x + right, y + down)))

  let array_restrict t (x, y) = (x % Array.length t.(0), y)

  let test_slope ~down ~right t =
    slope_coords ~down ~right (Array.length t - 1)
    |> Sequence.map ~f:(array_restrict t)
    |> Sequence.count ~f:(fun (x, y) -> t.(y).(x))

  let part1 (input : input) : output = test_slope ~down:1 ~right:3 input

  let part2 (input : input) : output =
    [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
    |> List.map ~f:(fun (right, down) -> test_slope ~down ~right input)
    |> List.reduce_exn ~f:( * )
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
