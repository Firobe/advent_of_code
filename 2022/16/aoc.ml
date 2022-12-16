open Aoclib

module Types = struct
  type node = { flow : int; neighbors : string list } [@@deriving show]
  type input = (string * node) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types
module G = Graph.Persistent.Graph.Concrete (String)

module W = struct
  include Int

  type edge = G.E.t

  let weight _ = 1
end

module GJ = Graph.Path.Johnson (G) (W)

module Parsing = struct
  open Angstrom
  open Parsing

  let name = take_while Base.Char.is_alpha

  let node =
    lift3
      (fun name flow neighbors -> (name, { flow; neighbors }))
      (string "Valve " *> name)
      (string " has flow rate=" *> integer)
      ((string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
      *> sep_by (string ", ") name)
    <* end_of_line

  let input = many node
end

module Solving = struct
  open Base

  let all_distances input =
    List.fold input ~init:G.empty ~f:(fun init (name, { neighbors; _ }) ->
        List.fold ~init neighbors ~f:(fun g n -> G.add_edge g name n))
    |> GJ.all_pairs_shortest_paths

  let interesting_nodes input =
    List.filter_map input ~f:(fun (name, { flow; _ }) ->
        if flow > 0 then Some (name, flow) else None)

  let search input =
    let distances = all_distances input in
    let rec aux time_left valves_left score pos =
      if time_left < 0 then score
      else
        List.map valves_left ~f:(fun (next, flow) ->
            let time = time_left - GJ.HVV.find distances (pos, next) - 1 in
            let score = score + max 0 (time * flow) in
            let left =
              List.filter valves_left ~f:(fun (s, _) ->
                  not (String.equal s next))
            in
            aux time left score next)
        |> List.max_elt ~compare
        |> Option.value ~default:score
    in
    aux 30 (interesting_nodes input) 0 "AA"

  let part1 (input : input) : output = search input
  let part2 (input : input) : output = List.length input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
