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
    let flows = Hashtbl.create (module String) in
    let set =
      List.filter_map input ~f:(fun (name, { flow; _ }) ->
          if flow > 0 then (
            Hashtbl.add_exn flows ~key:name ~data:flow;
            Some name)
          else None)
      |> Set.of_list (module String)
    in
    (set, flows)

  let search input =
    let distances = all_distances input in
    let valves, flows = interesting_nodes input in
    let rec aux time_left valves_left score pos =
      if time_left < 0 then score
      else
        Set.map
          (module Int)
          valves_left
          ~f:(fun next ->
            let time = time_left - GJ.HVV.find distances (pos, next) - 1 in
            let score = score + max 0 (time * Hashtbl.find_exn flows next) in
            let left = Set.remove valves_left next in
            aux time left score next)
        |> Set.max_elt
        |> Option.value ~default:score
    in
    aux 30 valves 0 "AA"

  let search2 input =
    let distances = all_distances input in
    let valves, flows = interesting_nodes input in
    let rec aux time_left valves_left score (pos1, pos2) =
      let go_next pos mode =
        Set.filter_map
          (module Int)
          valves_left
          ~f:(fun next ->
            let dist = GJ.HVV.find distances (pos, next) + 1 in
            let time = time_left - dist in
            if time < 0 then None
            else
              let score = score + max 0 (time * Hashtbl.find_exn flows next) in
              let left = Set.remove valves_left next in
              let pos = `Moving (next, dist) in
              let next_pos =
                match mode with `Left -> (pos, pos2) | `Right -> (pos1, pos)
              in
              Some (aux time_left left score next_pos))
        |> Set.max_elt
        |> Option.value_or_thunk ~default:(fun () ->
               match mode with
               | `Left -> aux time_left valves_left score (`Stuck, pos2)
               | `Right -> aux time_left valves_left score (pos1, `Stuck))
      in
      if time_left < 0 then score
      else
        match (pos1, pos2) with
        | `Moving (n1, d1), `Moving (n2, d2) ->
            let pos1, pos2, delta =
              if d1 = d2 then (`At n1, `At n2, d1)
              else if d1 > d2 then (`Moving (n1, d1 - d2), `At n2, d2)
              else (`At n1, `Moving (n2, d2 - d1), d1)
            in
            aux (time_left - delta) valves_left score (pos1, pos2)
        | `Stuck, `Moving (n, d) ->
            aux (time_left - d) valves_left score (pos1, `At n)
        | `Moving (n, d), `Stuck ->
            aux (time_left - d) valves_left score (`At n, pos2)
        | `At pos, _ -> go_next pos `Left
        | _, `At pos -> go_next pos `Right
        | `Stuck, `Stuck -> score
    in
    aux 26 valves 0 (`At "AA", `At "AA")

  let part1 (input : input) : output = search input
  let part2 (input : input) : output = search2 input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
