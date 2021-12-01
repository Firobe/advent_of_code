let rec sep_layers x y s n =
  if n >= String.length s then []
  else (String.sub s n (x * y) |> String.to_seq) :: (sep_layers x y s (n + x * y))

let occ_of x l =
  Seq.fold_left (fun acc e -> acc + if e = x then 1 else 0) 0 l

let findmin0 l =
  let _, x = List.fold_left (fun (v,ll) l ->
      let r = occ_of '0' l in
      if r < v then (r,l) else (v,ll)
    ) (99999999, Seq.empty) l in x

let rec get x y = function
  | [] -> assert false
  | h :: t ->
    begin match (String.of_seq h).[y * 25 + x] with
      | '2' -> get x y t
      | '0' -> ' '
      | '1' -> '#'
      | _ -> assert false
    end

let _ =
    let chan = open_in "input" in
    let raw = Std.input_list chan |> List.hd in
    let l = sep_layers 25 6 raw 0 in
    for y = 0 to 5 do
      for x = 0 to 24 do
        print_char (get x y l);
      done;
      print_newline ();
    done;
    close_in chan
