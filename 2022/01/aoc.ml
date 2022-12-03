open Aoclib

module Types = struct
  type input = int list list [@@deriving show]
  type output = int option [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = sep_by end_of_line (many (integer <* end_of_line))
end

module Solving = struct
  open Base

  let part1 (input : input) : output =
    List.map ~f:(List.sum ~f:Fn.id (module Int)) input |> List.max_elt ~compare

  let part2 (input : input) : output =
    let sorted_sums =
      List.map ~f:(List.sum ~f:Fn.id (module Int)) input
      |> List.sort ~compare |> List.rev
    in
    List.sum (module Int) ~f:Fn.id (List.take sorted_sums 3) |> Option.some
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
