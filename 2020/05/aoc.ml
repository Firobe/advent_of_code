open Aoclib

module Types = struct
  type range = { min : int; max : int } [@@deriving show { with_path = false }]

  type 'a seat = { row : 'a; column : 'a }
  [@@deriving show { with_path = false }]

  type input = int seat list [@@deriving show]
  type output = int option [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  (*open Parsing*)

  let one seat =
    let upper r =
      let len = r.max - r.min + 1 in
      { r with min = r.min + (len / 2) }
    in
    let lower r =
      let len = r.max - r.min + 1 in
      { r with max = r.max - (len / 2) }
    in
    char 'F' <|> char 'B' <|> char 'L' <|> char 'R' >>| function
    | 'F' -> { seat with row = lower seat.row }
    | 'B' -> { seat with row = upper seat.row }
    | 'L' -> { seat with column = lower seat.column }
    | 'R' -> { seat with column = upper seat.column }
    | _ -> assert false

  let init = { row = { min = 0; max = 127 }; column = { min = 0; max = 7 } }
  let unique { min; max } = if min = max then min else failwith "Ambiguous"

  let seat =
    one init >>= one >>= one >>= one >>= one >>= one >>= one >>= one >>= one
    >>= one
    >>| fun { row; column } -> { row = unique row; column = unique column }

  let input = many1 (seat <* end_of_line)
end

module Solving = struct
  open Base

  let id { row; column } = (row * 8) + column

  let part1 (input : input) : output =
    List.map input ~f:id |> List.max_elt ~compare:Int.compare

  let rec find = function
    | h1 :: h2 :: t -> if h2 <> h1 + 1 then Some (h1 + 1) else find (h2 :: t)
    | _ -> None

  let part2 (input : input) : output =
    List.map input ~f:id |> List.sort ~compare:Int.compare |> find
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
