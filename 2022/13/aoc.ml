open Aoclib

module Types = struct
  type packet = Leaf of int | Node of packet list
  [@@deriving show { with_path = false }]

  type input = (packet * packet) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let leaf x = Leaf x
  let node x = Node x

  let packet =
    fix (fun m ->
        integer >>| leaf
        <|> (char '[' *> sep_by (char ',') m <* char ']' >>| node))
    <* end_of_line

  let input = sep_by end_of_line (both packet packet)
end

module Solving = struct
  open Base

  let rec compare_packets a b =
    match (a, b) with
    | Leaf a, Leaf b -> compare a b
    | Leaf _, Node _ -> compare_packets (Node [ a ]) b
    | Node _, Leaf _ -> compare_packets a (Node [ b ])
    | Node ([] as a), Node b | Node a, Node ([] as b) ->
        compare (List.length a) (List.length b)
    | Node (a :: t1), Node (b :: t2) ->
        let c = compare_packets a b in
        if c = 0 then compare_packets (Node t1) (Node t2) else c

  let part1 (input : input) : output =
    let well_ordered (a, b) = compare_packets a b < 0 in
    List.filter_mapi input ~f:(fun i p ->
        if well_ordered p then Some (i + 1) else None)
    |> List.sum (module Int) ~f:Fn.id

  let part2 (input : input) : output =
    let div1 = Node [ Node [ Leaf 2 ] ] in
    let div2 = Node [ Node [ Leaf 6 ] ] in
    let equal a _ b = compare_packets a b = 0 in
    let sorted =
      div1 :: div2 :: List.concat_map input ~f:(fun (a, b) -> [ a; b ])
      |> List.sort ~compare:compare_packets
    in
    let p1, _ = List.findi_exn sorted ~f:(equal div1) in
    let p2, _ = List.findi_exn sorted ~f:(equal div2) in
    (p1 + 1) * (p2 + 1)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
