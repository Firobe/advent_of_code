open Aoclib

module Types = struct
  type input = char list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  (*open Parsing*)

  let input = (take_while Base.Char.is_alpha >>| Base.String.to_list) <* end_of_line
end

module Solving = struct
  open Base

  let find_uniq_seq size l =
    let rec aux pos l =
      let seq, tail = List.split_n l size in
      if List.length seq = size then
        let uniq = List.dedup_and_sort seq ~compare:Char.compare in
        if List.length uniq = size then pos
        else aux (pos + 1) (List.tl_exn seq @ tail)
      else failwith "No begin marker"
    in aux size l

  let part1 (input : input) : output = find_uniq_seq 4 input
  let part2 (input : input) : output = find_uniq_seq 14 input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
