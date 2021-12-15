open Base

(* just an experiment *)
module Pair
    (A : sig
       include Comparable.S
       include Sexpable.S with type t := t
     end)
    (B : sig
       include Comparable.S
       include Sexpable.S with type t := t
     end) = struct
  module O = struct
    type t = A.t * B.t
    let compare (a1,b1) (a2,b2) =
      let c = A.compare a1 a2 in if c = 0 then B.compare b1 b2 else c
    let sexp_of_t (a, b) = Sexp.List [A.sexp_of_t a; B.sexp_of_t b]
  end
  include O
  include Comparable.Make(O)
  include Comparator.Make(O)
end

module PQ = Psq.Make(Pair(Int)(Int))(Int)

let shortest_path size (risk : int * int -> int) src dst =
  let prio = PQ.sg src 0 in
  let visited = Set.Poly.empty in
  let rec aux visited prio =
    match PQ.pop prio with
    | None -> failwith "No path found"
    | Some ((next, weight), prio) ->
      if Poly.(next = dst) then weight
      else
        let visited = Set.Poly.add visited next in
        let prio =
          [(1, 0); (0, 1); (-1, 0); (0, -1)] 
          |> List.map ~f:(fun (dx, dy) -> (fst next + dx, snd next + dy))
          |> List.filter ~f:(fun (x, y) ->
              x >= 0 && y >= 0 && x < size && y < size)
          |> List.fold ~init:prio ~f:(fun p ((x,y) as n) ->
              let nw = weight + risk (x, y) in
              if Set.Poly.mem visited n then p
              else PQ.update n (function
                  | None -> Some nw
                  | Some ew when ew > nw -> Some nw
                  | Some x -> Some x
                ) p
            )
        in aux visited prio
  in aux visited prio

let solve1 l = 
  let len = Array.length l in
  let risk (x, y) = l.(y).(x) in
  shortest_path len risk (0, 0) (len - 1, len - 1)

let solve2 l =
  let len = Array.length l * 5 in
  let tile = Array.length l in
  let risk (x, y) =
    let orig = l.(y % tile).(x % tile) - 1 in
    (orig + x / tile + y / tile) % 9 + 1
  in
  shortest_path len risk (0, 0) (len - 1, len - 1)

let convert_data (l : string list) : int array array =
  Array.of_list_map ~f:(fun l ->
      String.to_list l |> Array.of_list_map ~f:(Fn.compose Int.of_string String.of_char)
    ) l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"