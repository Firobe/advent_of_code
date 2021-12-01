type coord = {x : int; y : int}
let bounding_box cl =
    let a, b = List.fold_left (fun (nw, se) c ->
        let nw' = match nw with
        | None -> Some c
        | Some c' -> Some {x = min c'.x c.x; y = min c'.y c.y}
        in
        let se' = match se with
        | None -> Some c
        | Some c' -> Some {x = max c'.x c.x; y = max c'.y c.y}
        in
        (nw', se')
    ) (None, None) cl in
    (Option.get a, Option.get b)

let mandist a b = abs (a.x - b.x) + abs (a.y - b.y)
let closest cl co =
    let (c, t) = List.fold_left (fun (closest, tie) nt ->
        match closest with
        | None -> Some nt, false
        | Some clo when mandist nt co < mandist clo co -> Some nt, false
        | Some clo when mandist nt co = mandist clo co -> Some nt, true
        | _ -> (closest, tie)
    ) (None, false) cl in
    if t then None else c

let genmap infs comp =
    let h = Hashtbl.create 100 in
    for x = 0 to Array.length comp - 1 do
        for y = 0 to Array.length comp.(x) - 1 do
            let c = comp.(x).(y) in
            match c with
            | None -> ()
            | Some c' when List.mem c' infs -> ()
            | Some c' -> begin
                let n = match Hashtbl.find_opt h c' with
                | None -> 0
                | Some v -> v + 1 in
                Hashtbl.replace h c' n
            end
        done
    done; h

let sum_mandist c cl =
    List.fold_left (fun old c' -> old + mandist c c') 0 cl

let pp_coord chan c =
    Printf.fprintf chan "(%d, %d)" c.x c.y

let in_infs nw se cl =
    let rec aux t step cur =
        if cur = t then []
        else 
            let next = {x=cur.x + step.x;y=cur.y+step.y} in
            let re = aux t step next in
            match closest cl cur with
            | None -> re
            | Some c when List.mem c re -> re
            | Some c -> c :: re
    in
    (aux {x=se.x+1;y=nw.y-1} {x=1;y=0} {x=nw.x;y=nw.y-1}) @ (* up *)
    (aux {x=se.x;y=se.y+1} {x=1;y=0} {x=nw.x+1;y=se.y+1}) @ (* down *)
    (aux {x=nw.x-1;y=se.y+1} {x=0;y=1} {x=nw.x-1;y=nw.y}) @ (* left *)
    (aux {x=se.x+1;y=se.y+1} {x=0;y=1} {x=se.x+1;y=nw.y}) (* right *)

let main1 () =
    let chan = open_in "input" in
    let lines = Std.input_list chan in
    let coords = List.map (fun s ->
        Scanf.sscanf s "%d, %d" (fun x y -> {x; y})
    ) lines in
    let nw, se = bounding_box coords in
    let infs = in_infs nw se coords in
    let mat = Array.make_matrix (se.x - nw.x) (se.y - nw.y)  None in
    let comp = Array.mapi (fun x om -> Array.mapi 
        (fun y _ -> closest coords {x=x+nw.x; y=y+nw.y}) om) mat in
    let m = genmap infs comp in
    let r = Hashtbl.fold (fun _ v old -> max v old) m 0 in
    Printf.printf "%d\n" r

let main2 () =
    let thr = 32 in
    let chan = open_in "input" in
    let lines = Std.input_list chan in
    let coords = List.map (fun s ->
        Scanf.sscanf s "%d, %d" (fun x y -> {x; y})
    ) lines in
    let nw, se = bounding_box coords in
    let bigb = thr / (List.length coords) + 100 in
    let tot = ref 0 in
    for x = nw.x - bigb to se.x + bigb do
        for y = nw.y - bigb to se.y + bigb do
            if sum_mandist {x; y} coords < thr then incr tot
        done
    done;
    Printf.printf "%d\n" !tot

let main = main2 ()
