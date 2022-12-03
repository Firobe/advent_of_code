open Aoclib

module CBag = struct
  include Bag.Make (struct 
      type t = char
      let compare = Char.compare
    end)
  let pp fmt t =
    let open Format in
    pp_print_string fmt "{";
    iter (fun c i -> fprintf fmt "%c:%d@ " c i) t;
    pp_print_string fmt "}"
end

module Types = struct
  type bag = {left: CBag.t; right: CBag.t}
  [@@deriving show]
  type input = bag list
  [@@deriving show]

  type output = int
  [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom

  let item = satisfy (function 'a'..'z' | 'A'..'Z' -> true | _ -> false)

  let create_bag str =
    let f bag c = CBag.add c bag in
    String.fold_left f CBag.empty str

  let create_bags str =
    let cutoff = (String.length str) / 2 in
    let left = String.sub str 0 cutoff |> create_bag in
    let right = String.sub str cutoff cutoff |> create_bag in
    {left; right}

  let bag = (consumed (many1 item) >>| create_bags) <* end_of_line

  let input = many bag
end

module Solving = struct
  open Base

  let priority c = match c with
    | 'a'..'z' -> Char.to_int c - Char.to_int 'a' + 1
    | 'A'..'Z' -> Char.to_int c - Char.to_int 'A' + 27
    | _ -> assert false

  let merge_bag b = CBag.union b.left b.right
  let prio_singleton b = CBag.choose b |> fst |> priority

  let part1 (input : input) : output =
    input
    |> List.map ~f:(fun bag -> CBag.inter bag.left bag.right)
    |> List.sum (module Int) ~f:prio_singleton

  let part2 (input : input) : output =
    input
    |> List.chunks_of ~length:3
    |> List.map ~f:(function
        |[a; b; c] ->
          CBag.(inter (inter (merge_bag a) (merge_bag b)) (merge_bag c))
        | _ -> assert false
      )
    |> List.sum (module Int) ~f:prio_singleton
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ~debug:true ()
