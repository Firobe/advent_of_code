let sc a b = Char.lowercase_ascii a = Char.lowercase_ascii b

let rec pass seen todo = match (seen, todo) with
    | _, [] -> seen
    | [], h :: t -> pass [h] t
    | sh :: st, th :: tt when sh <> th && sc sh th -> pass st tt
    | _, h :: t -> pass (h :: seen) t

let rec search l i =
    if i < 65 then None else
    let ml = search l (i - 1) in
    let fl = List.filter (fun c -> not (sc (Char.chr i) c)) l in
    let rl = pass [] fl in
    let len = List.length rl - 1 in
    Some (min len (Option.default len ml))

let _ =
    let chan = open_in "input" in
    let l = ExtString.String.explode (Std.input_all chan) in
    let r = Option.get @@ search l 90 in
    Printf.printf "%d\n" r
