let iob b = if b then 1 else 0

type guard = int
type event =
    | Shift of guard
    | Wakes
    | Sleeps
type log = int list * event

let prst l =
    Printf.printf "[";
    List.iter (Printf.printf "%d ") l;
    Printf.printf "]\n"

let h = Hashtbl.create 10 (* Bind guard id to sleep schedule array *)

let sleepfor guard f t =
    let ar = match Hashtbl.find_opt h guard with
    | None -> 
        let r = Array.make 60 0 in
        Hashtbl.add h guard r; r
    | Some t -> t
    in
    for i = f to t do ar.(i) <- ar.(i) + 1 done

(* false : awake *)
let replay (guard, state) = function
    | [_;_;_;_;m], e ->
            begin match e with
            | Sleeps -> (guard, Some m)
            | Wakes -> begin match state with
                        | None -> assert false
                        | Some ls ->
                                sleepfor guard ls (m - 1);
                            (guard, None)
                        end
            | Shift newg -> begin match state with
                        | None -> (newg, None)
                        | Some _ -> assert false
                            end
            end
    | _ -> assert false

let maxiar ar  =
    let rec aux i =
        if i < 0 then (0, 0) else
        let (mi, mv) = aux (i - 1) in
        if ar.(i) > mv then (i, ar.(i))
        else (mi, mv)
    in aux (Array.length ar - 1)

let _ =
    let chan = open_in "input" in
    let l = (Std.input_list chan) in
    let (events : log list) = List.map (fun s ->
        let wl = String.split_on_char ' ' s in
        let stamp = Scanf.sscanf ((List.hd wl)^" "^(List.nth wl 1))
        "[%d-%d-%d %d:%d]" (fun a b c d e -> [a;b;c;d;e]) in
        let event = match (List.nth wl 2) with
        | "wakes" -> Wakes
        | "falls" -> Sleeps
        | "Guard" -> Scanf.sscanf (List.nth wl 3) "#%d" (fun a -> Shift a)
        | _ -> assert false
        in (stamp, event)
    ) l in
    let sevents = List.sort (fun (s1, _) (s2, _) -> compare s1 s2) events in
    ignore (List.fold_left replay (-1, None) sevents);
    let aux g sched (mg, (mmi, mmv)) =
        let (nm, nv) = maxiar sched in
        if nv > mmv then (g, (nm, nv))
        else (mg, (mmi, mmv))
    in
    let (g, (m, _)) = Hashtbl.fold aux h (0, (0, 0)) in
    Printf.printf "%d %d %d\n" g m (g * m)
