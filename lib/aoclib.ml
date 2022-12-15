module type Types = sig
  type input
  type output

  val pp_input : Format.formatter -> input -> unit
  val pp_output : Format.formatter -> output -> unit
end

module type Parsing = sig
  type input

  val input : input Angstrom.t
end

module type Solving = sig
  type input
  type output

  val part1 : input -> output
  val part2 : input -> output
end

module Parsing = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let neg_integer =
    lift2
      (fun sign n -> if sign = '-' then -n else n)
      (Angstrom.option '+' (char '-'))
      integer
end

module MakeDay
    (T : Types)
    (P : Parsing with type input := T.input)
    (S : Solving with type input := T.input and type output := T.output) =
struct
  let do_parse str =
    let open Angstrom in
    let open Buffered in
    let state = parse P.input in
    let state = feed state (`String str) in
    let end_state = feed state `Eof in
    let result = state_to_result end_state in
    let unconsumed =
      match state_to_unconsumed end_state with
      | None -> None
      | Some { buf; off; len } ->
          if len = 0 then None else Some (Bigstringaf.substring buf ~off ~len)
    in
    let result =
      match (result, unconsumed) with
      | x, None -> x
      | Ok _, Some u -> Error (Printf.sprintf "unconsumed: '%s'" u)
      | Error msg, Some u ->
          Error (Printf.sprintf "%s / unconsumed: '%s'" msg u)
    in
    match result with Ok v -> v | Error msg -> failwith msg

  let go debug file =
    Format.printf "@.%s@.%!" file;
    let input = Stdio.In_channel.read_all file |> do_parse in
    let t1 = Unix.gettimeofday () in
    let o1 = S.part1 input in
    let t2 = Unix.gettimeofday () in
    let o2 = S.part2 input in
    let t3 = Unix.gettimeofday () in
    Format.printf "Part 1: %a in %fs@.%!" T.pp_output o1 (t2-.t1);
    Format.printf "Part 2: %a in %fs@.%!" T.pp_output o2 (t3-.t2);
    if debug then Format.printf "Parsed:@[%a@]@.%!" T.pp_input input

  let run_all () =
    let debug1 = Array.length Sys.argv > 1 && Sys.argv.(1) = "-d" in
    let debug2 = Array.length Sys.argv > 2 && Sys.argv.(2) = "-d" in
    go debug1 "example";
    go debug2 "input"
end
