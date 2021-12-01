let debug = false

type cell =
  { get : unit -> int;
    set : int -> unit
  }

let plist chan l =
  Printf.fprintf chan "[ ";
  List.iter (Printf.fprintf chan "%d ") l;
  Printf.fprintf chan "]"

let execute prog input output =
  let mem = Array.copy prog in
  let rec step pc =
    let opcode = mem.(pc) mod 100 in
    let len = match opcode with
      | 1 | 2 | 7 | 8 -> 3
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | 99 -> 0
      | _ -> failwith ("Wrong instruction " ^ (string_of_int opcode))
    in
    let rec dtl x n =
      if n = 0 then []
      else (x mod 10) :: (dtl (x / 10) (n - 1))
    in
    let modes = dtl (mem.(pc) / 100) len in
    let vals = Array.sub mem (pc + 1) len |> Array.to_list in
    if debug then Printf.printf "OP %d MODES %a VALS %a\n" opcode plist modes plist vals;
    let params = List.map2 (fun v m -> match m with
        | 0 -> {get = (fun () -> mem.(v)); set = fun x -> mem.(v) <- x}
        | 1 -> {get = (fun () -> v); set = fun _ -> failwith "Trying to write to immediate"}
        | _ -> failwith "Wrong parameter mode"
      ) vals modes |> Array.of_list in
    let pg x = params.(x).get () in
    let ps x y = params.(x).set y in
    match opcode with
    | 1 ->
      ps 2 (pg 0 + pg 1);
      step (pc + len + 1)
    | 2 ->
      ps 2 (pg 0 * pg 1);
      step (pc + len + 1)
    | 3 ->
      ps 0 (input ());
      step (pc + len + 1)
    | 4 ->
      output (pg 0);
      step (pc + len + 1)
    | 5 ->
      if pg 0 <> 0 then
        step (pg 1)
      else step (pc + len + 1)
    | 6 ->
      if pg 0 = 0 then
        step (pg 1)
      else step (pc + len + 1)
    | 7 ->
      ps 2 (if pg 0 < pg 1 then 1 else 0);
      step (pc + len + 1)
    | 8 ->
      ps 2 (if pg 0 = pg 1 then 1 else 0);
      step (pc + len + 1)
    | 99 -> ()
    | _ -> failwith "Wrong instruction"
  in
  step 0

let amplify prog seq v =
  let out = ref 0 in
  let first = ref true in
  let input () =
    if !first then (first := false; seq) else v
  in execute prog input (fun x -> out := x);
  !out

let rec chain prog input = function
  | [] -> input
  | h :: t -> chain prog (amplify prog h input) t

let rec interleave x lst = 
  match lst with
  | [] -> [[x]]
  | hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl))

(*permutations [1; 2; 3] = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)
let rec permutations lst = 
  match lst with
  | hd::tl -> List.concat (List.map (interleave hd) (permutations tl))
  | _ -> [lst]

let _ =
    let chan = open_in "input" in
    let prog = Std.input_list chan |> List.hd |>
               Str.split (Str.regexp ",") |> List.map int_of_string |>
               Array.of_list in
    let permuts = permutations [0;1;2;3;4] in
    let r = List.fold_left (fun acc l -> max acc (chain prog 0 l)) 0 permuts in
    print_int r;
    print_newline ();
    close_in chan
