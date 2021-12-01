open ExtString

let cti c = if c = '#' then 1 else 0

let rec vals tab = function
  | a :: ((b :: c :: d :: e :: _) as t) ->
    let comb = e + d * 2 + c * 4 + b * 8 + a * 16 in
    tab.(comb) :: (vals tab t)
  | _ -> []

let rec trim_left = function
  | [] -> [], 0
  | 0 :: t ->
    let (vt, nt) = trim_left t in (vt, nt + 1)
  | l -> l, 0

let rec trim_right = function
  | [] -> []
  | 0 :: t ->
    let trimmed = trim_right t in
    if t = [] then [] else 0 :: trimmed
  | _ :: t ->
    1 :: (trim_right t)

let rec sum i = function
  | [] -> 0
  | h :: t ->
    h * i + sum (i + 1) t

let h = Hashtbl.create 1000

let off = ref 0
let rec step tab state n =
  if n = 0 then state,0 else (
    let padded = [0;0;0] @ state @ [0;0;0] in
    let v = vals tab padded in
    let tlv, noff = trim_left v in
    let trv = trim_right tlv in
    off := !off - 1 + noff;
    (match Hashtbl.find_opt h trv with
     | None -> Hashtbl.add h trv (n,!off);
        step tab trv (n-1)
    | Some (vn, voff) -> (
      Printf.printf "cycle found %d from (%d,%d) (s %d v %d)\n%!" n vn voff
        (List.length trv) (sum !off trv);
      trv,n)
    )
  )  

let gen = 50000000000
let main =
  Printexc.record_backtrace true;
  let chan = open_in "input" in
  let initial = Scanf.sscanf (input_line chan) "initial state: %s" Std.identity
                |> String.explode |> List.map cti in
  List.iter print_int initial; print_newline ();
  ignore @@ input_line chan;
  let combs = chan
              |> Std.input_list
              |> List.map (fun s -> Scanf.sscanf s "%s => %c" (fun a b -> (a,b)))
              |> List.map (fun (c, o) ->
                  let oo = cti o in
                  let v = String.fold_left (fun ac bc ->
                      2 * ac + cti bc
                    ) 0 c in (v, oo)
                )
  in
  let tab = Array.init 32 (fun i -> match List.assoc_opt i combs with
      | None -> 0
      | Some v -> v
    ) in
  let final, fn = (step tab initial gen) in
  Printf.printf "Off now %d\n%!" !off;
  off := !off + fn - 1;
  let s = sum !off final in
  print_int s; print_newline ()
