let rec fuel mass =
  let f = mass / 3 - 2 in
  if f <= 0 then 0
  else f + fuel f

let _ =
    let chan = open_in "input" in
    let l = List.map int_of_string (Std.input_list chan) in
    let r = List.fold_left (fun acc mass -> acc + fuel mass) 0 l in
    Printf.printf "%d\n" r
