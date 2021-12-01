let iob b = if b then 1 else 0
let m = Array.make_matrix 2000 2000 0

let posit (_,x,y,w,h) =
    for i = x to x + w - 1 do
        for j = y to y + h - 1 do
            m.(i).(j) <- m.(i).(j) + 1
        done
    done

let nocol (_,x,y,w,h) =
    Array.for_all (fun ar -> Array.for_all ((=) 1) (Array.sub ar y h)) (Array.sub m x w)

let _ =
    let chan = open_in "input" in
    let l = (Std.input_list chan) in
    let claims = List.map (fun s ->
        Scanf.sscanf s "#%d @ %d,%d: %dx%d"
        (fun a b c d e -> (a, b, c, d, e))
    ) l in
    List.iter posit claims;
    let id, _, _, _, _ = List.find nocol claims in
    Printf.printf "#%d\n" id

