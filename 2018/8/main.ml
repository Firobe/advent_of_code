open ExtList
open ExtString

type tree = {
    children : tree list;
    metadata : int list
}

let rec parse_children n data =
    if n = 0 then ([], data)
    else
        let c, left = parse data in
        let fc, fleft = parse_children (n-1) left in
        (c :: fc, fleft)
and parse = function
    | cn :: mn :: t ->
        let children, left = parse_children cn t in
        let metadata, final = List.split_nth mn left in
        {children; metadata}, final
    | _ -> assert false

let rec sum_meta t =
    let loc_sum = List.fold_left (+) 0 t.metadata in
    List.fold_left (+) 0 (List.map sum_meta t.children) + loc_sum

let rec sum_index = function
    | {children = []; metadata} -> List.fold_left (+) 0 metadata
    | {children; metadata} ->
        let cv = List.map sum_index children in
        List.fold_left (fun old index ->
            let lv = match List.nth_opt cv (index - 1) with
                | None -> 0
                | Some v -> v
            in old + lv
        ) 0 metadata

let main =
    let chan = open_in "input" in
    let str = Std.input_all chan |> String.trim in
    let data = String.nsplit str " " |> List.map int_of_string in
    let tree, _ = parse data in
    let r = sum_index tree in
    print_int r; print_newline ()
