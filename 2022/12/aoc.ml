open Aoclib

module Types = struct
  type input = char list list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom

  let input = many (many_till any_char end_of_line)
end

module Solving = struct
  open Base

  module Coord = struct
    type t = { x : int; y : int } [@@deriving eq, ord, hash]
  end

  module G = Graph.Persistent.Digraph.ConcreteBidirectional (Coord)

  module W = struct
    type edge = G.E.t
    type t = int

    let weight = Fn.(const 1)
    let compare = compare
    let add = ( + )
    let zero = 0
  end

  module GD = Graph.Path.Dijkstra (G) (W)
  module GB = Graph.Path.BellmanFord (G) (W)
  module GO = Graph.Oper.P (G)

  let find_se input =
    let open Coord in
    let elevation x = Char.to_int x - Char.to_int 'a' in
    let (s, e), g =
      List.fold_mapi input ~init:(None, None) ~f:(fun x init ->
          List.fold_mapi ~init ~f:(fun y (s, e) -> function
            | 'S' -> ((Some { x; y }, e), ({ x; y }, elevation 'a'))
            | 'E' -> ((s, Some { x; y }), ({ x; y }, elevation 'z'))
            | c -> ((s, e), ({ x; y }, elevation c))))
    in
    (Option.value_exn s, Option.value_exn e, g)

  let build_graph a =
    let rec line g = function
      | (a, e1) :: (b, e2) :: t ->
          let g = if e1 + 1 >= e2 then G.add_edge g a b else g in
          let g = if e2 + 1 >= e1 then G.add_edge g b a else g in
          line g ((b, e2) :: t)
      | _ -> g
    in
    let grid l g = List.fold l ~init:g ~f:line in
    G.empty |> grid a |> grid (List.transpose_exn a)

  let part1 (input : input) : output =
    let s, e, g = find_se input in
    let g = build_graph g in
    let _, length = GD.shortest_path g s e in
    length

  let part2 (input : input) : output =
    let _, e, gr = find_se input in
    let g = build_graph gr |> GO.mirror in
    let h = GB.all_shortest_paths g e in
    List.concat_map gr
      ~f:
        (List.filter_map ~f:(fun (a, e) ->
             if e = 0 then GB.H.find_opt h a else None))
    |> List.min_elt ~compare |> Option.value_exn
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
