open Aoclib

module Types = struct
  type instr = Noop | Addx of int [@@deriving show { with_path = false }]
  type input = instr list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let neg_integer =
    lift2
      (fun sign n -> if sign = '-' then -n else n)
      (Angstrom.option '+' (char '-'))
      integer

  let instr =
    string "noop" *> return Noop
    <|> (string "addx " *> neg_integer >>| fun n -> Addx n)
    <* end_of_line

  let input = many1 instr
end

module Solving = struct
  open Base

  type status = Fetch | Do of instr
  type state = { status : status; instrs : instr list; x : int }

  let next_cycle state =
    match (state.status, state.instrs) with
    | Fetch, [] -> None
    | Fetch, Noop :: t -> Some { state with instrs = t }
    | Fetch, i :: t -> Some { status = Do i; instrs = t; x = state.x }
    | Do (Addx n), _ -> Some { state with status = Fetch; x = state.x + n }
    | _ -> assert false

  let exec_seq instrs =
    let init = { status = Fetch; x = 1; instrs } in
    Sequence.unfold ~init
      ~f:(Fn.compose Option.map ~f:(fun s -> (s.x, s)) next_cycle)
    |> Sequence.(append (of_list [ 1 ]))
    |> Sequence.mapi ~f:(fun i x -> (i + 1, x))

  let keep_some_cycles exec =
    Sequence.filter exec ~f:(fun (c, _) -> (c - 20) % 40 = 0)

  let sig_strength (i, x) = i * x

  let part1 (input : input) : output =
    exec_seq input |> keep_some_cycles
    |> (Fn.flip Sequence.take) 6
    |> Sequence.sum (module Int) ~f:sig_strength

  let visible (cycle, x) =
    let drawn = (cycle - 1) % 40 in
    if x - 1 <= drawn && drawn <= x + 1 then '#' else ' '

  let part2 (input : input) : output =
    exec_seq input |> Sequence.map ~f:visible
    |> Fn.flip Sequence.chunks_exn 40
    |> Sequence.map ~f:String.of_char_list
    |> Sequence.iter ~f:(Stdlib.Printf.printf "%s\n");
    0
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
