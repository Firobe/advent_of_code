open Base

(* Increase value associated to k in m, or initialise it *)
let increase_map ?(by=1) k m =
  Map.update m k ~f:(function Some x -> x + by | None -> by)

(* Mapping pairs of characters to their frequency in a string *)
let pair_map s =
  let chars = String.to_list s in
  List.map2_exn ~f:(fun x y -> String.of_char_list [x; y])
    (List.drop_last_exn chars)
    (List.tl_exn chars)
  |> List.fold ~init:(Map.empty (module String)) ~f:(Fn.flip increase_map)

(* Mapping single characters to their frequency in a string *)
let freq_map =
  String.fold ~init:(Map.empty (module String))
    ~f:(fun m c -> increase_map (String.of_char c) m)

(* new_pairs "AB" "C" = ("AC", "BC") *)
let new_pairs p i =
  ([p.[0]; i.[0]] |> String.of_char_list,
   [i.[0]; p.[1]] |> String.of_char_list)

(* Apply a step, only keeping track of pair and char frequencies *)
let step rules (pm, fm) =
  let f ~key ~data ((pm, fm) as p) =
    match Map.find rules key with
    | None -> p
    | Some insert ->
      let p1, p2 = new_pairs key insert in
      let pm =
        increase_map ~by:(-data) key pm
        |> increase_map ~by:data p1
        |> increase_map ~by:data p2 in
      (pm, increase_map ~by:data insert fm)
  in Map.fold pm ~init:(pm, fm) ~f

let solve n ((pm, fm), rules) =
  let _, fm = Fn.apply_n_times ~n (step rules) (pm, fm) in
  let common init f = Map.fold ~init ~f:(fun ~key:_ ~data cur -> f cur data) fm in
  (common 0 Int.max) - (common Int.max_value Int.min)

let solve1 = solve 10
let solve2 = solve 40

let convert_data (l : string list) =
  match l with
  | template :: "" :: rules ->
    let make_rule s = Caml.Scanf.sscanf s "%s -> %s"
        (fun pat insert -> (pat, insert)) in
    let rules = List.map rules ~f:make_rule 
                |> Map.of_alist_exn (module String) in
    (pair_map template, freq_map template), rules
  | _ -> failwith "Wrong input"

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
