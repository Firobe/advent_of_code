let debug = false

let bi = Z.of_int
let ib = Z.to_int
let (<+>) = Z.add
let (<*>) = Z.mul

type cell =
  { get : unit -> Z.t;
    set : Z.t -> unit
  }

let plist chan l =
  Printf.fprintf chan "[ ";
  List.iter (Printf.fprintf chan "%d ") l;
  Printf.fprintf chan "]"

let zplist chan l =
  Printf.fprintf chan "[ ";
  List.iter (fun x -> Printf.fprintf chan "%s " (Z.to_string x)) l;
  Printf.fprintf chan "]"


let execute prog input output =
  let mem = Array.make 1000000 (bi 0) in
  Array.blit prog 0 mem 0 (Array.length prog);
  let relative_base = ref 0 in
  let rec step pc =
    let opcode = (ib mem.(pc)) mod 100 in
    let len = match opcode with
      | 1 | 2 | 7 | 8 -> 3
      | 3 | 4 | 9 -> 1
      | 5 | 6 -> 2
      | 99 -> 0
      | _ -> failwith ("Wrong instruction " ^ (string_of_int opcode))
    in
    let rec dtl x n =
      if n = 0 then []
      else (x mod 10) :: (dtl (x / 10) (n - 1))
    in
    let modes = dtl ((ib mem.(pc)) / 100) len in
    let vals = Array.sub mem (pc + 1) len |> Array.to_list in
    if debug then Printf.printf "OP %d MODES %a VALS %a\n" opcode plist modes zplist vals;
    let params = List.map2 (fun v m -> match m with
        | 0 -> {get = (fun () -> mem.(ib v)); set = fun x -> mem.(ib v) <- x}
        | 1 -> {get = (fun () -> v); set = fun _ -> failwith "Trying to write to immediate"}
        | 2 -> {get = (fun () -> mem.((ib v) + !relative_base));
                set = fun x -> mem.((ib v) + !relative_base) <- x}
        | _ -> failwith "Wrong parameter mode"
      ) vals modes |> Array.of_list in
    let pg x = params.(x).get () in
    let ps x y = params.(x).set y in
    match opcode with
    | 1 ->
      ps 2 (pg 0 <+> pg 1);
      step (pc + len + 1)
    | 2 ->
      ps 2 (pg 0 <*> pg 1);
      step (pc + len + 1)
    | 3 ->
      ps 0 (input ());
      step (pc + len + 1)
    | 4 ->
      output (pg 0);
      step (pc + len + 1)
    | 5 ->
      if pg 0 <> (bi 0) then
        step (ib @@ pg 1)
      else step (pc + len + 1)
    | 6 ->
      if pg 0 = (bi 0) then
        step (ib @@ pg 1)
      else step (pc + len + 1)
    | 7 ->
      ps 2 (if pg 0 < pg 1 then (bi 1) else (bi 0));
      step (pc + len + 1)
    | 8 ->
      ps 2 (if pg 0 = pg 1 then (bi 1) else (bi 0));
      step (pc + len + 1)
    | 9 ->
      relative_base := !relative_base + (ib @@ pg 0);
      step (pc + len + 1)
    | 99 -> ()
    | _ -> failwith "Wrong instruction"
  in
  step 0

let _ =
    let chan = open_in "input" in
    let prog = Std.input_list chan |> List.hd |>
               Str.split (Str.regexp ",") |> List.map Z.of_string |>
               Array.of_list in
    let out = ref [] in
    let output x = out := !out @ [x] in
    execute prog (fun () -> (bi 2)) output;
    Printf.printf "[ ";
    List.iter (fun x -> Printf.printf "%s " (Z.to_string x)) !out;
    Printf.printf "]\n";
    close_in chan
