let execute prog p1 p2 =
  let mem = Array.copy prog in
  let rec step pc =
    let next () = (mem.(mem.(pc + 1)),
                   mem.(mem.(pc + 2)),
                   mem.(pc + 3)) in
    match mem.(pc) with
    | 1 ->
      let f1, f2, t = next () in
      mem.(t) <- f1 + f2;
      step (pc + 4)
    | 2 ->
      let f1, f2, t = next () in
      mem.(t) <- f1 * f2;
      step (pc + 4)
    | 99 -> ()
    | _ -> assert false
  in
  mem.(1) <- p1;
  mem.(2) <- p2;
  step 0;
  mem.(0)


let search prog =
  let range = 100 in
  let goal = 19690720 in
  let rec s1 i =
    let rec s2 j =
      if j = range then None
      else if (execute prog i j) = goal then
        Some j
      else s2 (j + 1)
    in
    if i = range then None
    else match s2 0 with
      | None -> s1 (i + 1)
      | Some j -> Some (i, j)
  in
  s1 0


let _ =
    let chan = open_in "input" in
    let prog = Std.input_list chan |> List.hd |>
               Str.split (Str.regexp ",") |> List.map int_of_string |>
               Array.of_list in
    begin match search prog with
    | None -> print_string "Nothing found !"
    | Some (a,b) -> Printf.printf "%d, %d -> %d" a b (100 * a + b)
    end ;
    print_newline ();
    close_in chan
