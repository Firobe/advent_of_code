open ExtString

module Key = struct
  type t = string
  let compare = compare
end

module EMap = Map.Make (Key)
module ESet = Set.Make (Key)

let parse s =
  let words = String.nsplit s " " in
  (List.nth  words 7, List.nth words 1)

let add_edge g (f,t) =
  EMap.update f (function
      | None -> Some [t]
      | Some l -> Some (t:: l)
    ) g

let tot s = Char.code s.[0] - 4

let rec findfirst bound = function
  | [] -> []
  | _ when bound = 0 -> []
  | (k, []) :: t -> k :: (findfirst (bound - 1) t)
  | _ :: t -> findfirst bound t

let rec dothething g workers =
  if EMap.is_empty g then 0
  else (
    let zeroed = workers |> List.filter (fun w -> snd w = 0) |> List.map fst in
    let newworkers = List.filter (fun w -> snd w <> 0) workers in
    let ug = List.fold_left (fun og ttr ->
        EMap.map (List.filter ((<>) ttr)) og
      ) g zeroed in
    let idle = 5 - List.length newworkers in
    let tr = ug |> EMap.bindings |> findfirst idle in
    let nw = newworkers @ List.map (fun v -> (v, tot v)) tr in
    List.iter (fun (a,b) -> Printf.printf "(%s, %d) " a b) nw;
    print_newline ();
    let ng = List.fold_left (fun og ttr -> EMap.remove ttr og) ug tr in
    let minstep = List.fold_left (fun o (_, s) -> min o s) 200 nw in
    let red = List.map (fun (t, v) -> (t, v - minstep)) nw in
    minstep + (dothething ng red)
  )

let main =
    let chan = open_in "input" in
    let edges = Std.input_list chan |> List.map parse in
    let graph = List.fold_left add_edge EMap.empty edges in
    let vert = List.fold_left (fun s (a,b) -> s |> ESet.add a |> ESet.add b)
        ESet.empty edges in
    let full_graph = ESet.fold (fun v g ->
        EMap.update v (function None -> Some [] | o -> o) g
      ) vert graph in
    let r = dothething full_graph [] in
    print_int r; print_newline ()
