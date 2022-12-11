open Aoclib

module Types = struct
  type monkey = {
    items : int list;
    op : int -> int;
    divisible : int;
    next_true : int; (* adjusted -1 *)
    next_false : int;
  }
  [@@warning "-69"] [@@deriving show { with_path = false }]

  type input = monkey list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let op =
    let v = string "old" *> return `Old <|> (integer >>| fun n -> `Int n) in
    let op = string " + " *> return ( + ) <|> string " * " *> return ( * ) in
    lift3
      (fun v1 op v2 x ->
        let v a = match a with `Old -> x | `Int n -> n in
        op (v v1) (v v2))
      v op v

  let monkey =
    string "Monkey " *> integer *> char ':' *> end_of_line
    *> string "  Starting items: "
    *> sep_by1 (string ", ") integer
    <* end_of_line
    >>= fun items ->
    lift4
      (fun op divisible next_true next_false ->
        { items; op; divisible; next_true; next_false })
      (string "  Operation: new = " *> op <* end_of_line)
      (string "  Test: divisible by " *> integer <* end_of_line)
      (string "    If true: throw to monkey " *> integer <* end_of_line)
      (string "    If false: throw to monkey " *> integer <* end_of_line)

  let input = sep_by1 end_of_line monkey
end

module Solving = struct
  open Base

  let process_item mode t i =
    let v =
      match mode with
      | `P1 -> t.op i / 3
      | `P2 -> t.op i % 223092870 (* big magic *)
    in
    let n = if v % t.divisible = 0 then t.next_true else t.next_false in
    (v, n)

  let go_monkey mode nb t queue =
    let to_dispatch = List.map t.items ~f:(process_item mode t) in
    let queue =
      List.mapi queue ~f:(fun i t' ->
          let to_add =
            List.filter_map to_dispatch ~f:(fun (w, id) ->
                if (i + nb + 1) % (List.length queue + 1) = id then Some w
                else None)
          in
          { t' with items = t'.items @ to_add })
    in
    (queue @ [ { t with items = [] } ], List.length to_dispatch)

  let round mode (monkeys, sums) =
    let rec aux n acc queue =
      if n >= List.length queue then (queue, acc)
      else
        let queue, r =
          go_monkey mode n (List.hd_exn queue) (List.tl_exn queue)
        in
        aux (n + 1) (r :: acc) queue
    in
    let queue, sums' = aux 0 [] monkeys in
    let sums = List.map2_exn sums sums' ~f:( + ) in
    (queue, sums)

  let go_rounds mode input n =
    let init = List.init (List.length input) ~f:(Fn.const 0) in
    Fn.apply_n_times ~n (round mode) (input, init)
    |> snd |> List.sort ~compare |> List.rev
    |> (Fn.flip List.take) 2
    |> List.reduce_exn ~f:( * )

  let part1 (input : input) : output = go_rounds `P1 input 20
  let part2 (input : input) : output = go_rounds `P2 input 10000
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
