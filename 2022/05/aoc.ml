open Aoclib

module Types = struct
  type move = { many : int; src : int; dst : int } [@@deriving show]

  let pp_array pp_elem fmt t =
    Array.to_seq t |> Format.(pp_print_seq pp_elem fmt)

  type crates = char list array [@@deriving show]
  type input = crates * move list [@@deriving show]
  type output = string [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let crate =
    char '[' *> satisfy Base.Char.is_alpha
    <* char ']'
    <|> (char ' ' *> char ' ' <* char ' ')

  let crate_line = sep_by (char ' ') crate <* end_of_line

  let crates =
    many1 crate_line
    <* skip_many (not_char '\n')
    <* end_of_line <* end_of_line
    >>| Base.(
          fun l ->
            List.transpose_exn l
            |> List.map ~f:(List.filter ~f:(Char.( <> ) ' '))
            |> Array.of_list)

  let move =
    lift3
      (fun many src dst -> { many; src; dst })
      (string "move " *> integer)
      (string " from " *> integer)
      (string " to " *> integer)
    <* end_of_line

  let input = both crates (many1 move)
end

module Solving = struct
  open Base

  let execute_move rev stacks { many; src; dst } =
    let rev = if rev then List.rev else Fn.id in
    let removed, new_stack = List.split_n stacks.(src - 1) many in
    stacks.(src - 1) <- new_stack;
    let new_stack' = rev removed @ stacks.(dst - 1) in
    stacks.(dst - 1) <- new_stack';
    stacks

  let execute ~rev stacks moves =
    List.fold moves ~init:stacks ~f:(execute_move rev)

  let get_top_string stacks =
    Array.map stacks ~f:List.hd_exn |> Array.to_list |> String.of_char_list

  let part1 ((stacks, moves) : input) : output =
    execute ~rev:true (Array.copy stacks) moves |> get_top_string

  let part2 ((stacks, moves) : input) : output =
    execute ~rev:false (Array.copy stacks) moves |> get_top_string
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
