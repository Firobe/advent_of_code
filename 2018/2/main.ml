let iob b = if b then 1 else 0

let diff s1 s2 =
    let rec aux i =
        if i < 0 then 0
        else iob (s1.[i] <> s2.[i]) + aux (i - 1)
    in aux (String.length s1 - 1)

let rec find = function
    | [] -> assert false
    | h1 :: t1 ->
        let rec aux = function
            | [] -> None
            | h2 :: _ when diff h1 h2 = 1 -> Some (h1, h2)
            | _ :: t2 -> aux t2
        in match aux t1 with
        | Some c -> c
        | None -> find t1

let printsame s1 s2 =
    let rec aux i =
        if i < (String.length s1) then (
            if s1.[i] = s2.[i] then Printf.printf "%c" s1.[i];
            aux (i + 1)
        )
    in aux 0

let _ =
    let chan = open_in "input" in
    let l = (Std.input_list chan) in
    let c1, c2 = find l in
    printsame c1 c2;
    Printf.printf "\n"

