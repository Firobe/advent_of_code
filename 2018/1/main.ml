let ha = Hashtbl.create 10000

let rec f cur = function
    | [] -> assert false
    | h :: t -> 
        if (Hashtbl.mem ha cur) then cur
        else  (
            Hashtbl.add ha cur true;
            f (h + cur) (t @ [h])
        )

let _ =
    let chan = open_in "input" in
    let l = List.map int_of_string (Std.input_list chan) in
    let r = f 0 l in
    Printf.printf "%d\n" r
