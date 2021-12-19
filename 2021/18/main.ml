open Base

type number = Regular of int | Pair of (number * number)
type zipper = {
  parents : (bool * number) list;
  current : number;
}

let move dir z = match dir, z with
  | `Up, {parents = (true, b) :: tl; _} ->
    Some {parents = tl; current = Pair (z.current, b)}
  | `Up, {parents = (false, b) :: tl; _} ->
    Some {parents = tl; current = Pair (b, z.current)}
  | `Left, {current=Pair (l, r); _} ->
    Some {parents = (true, r) :: z.parents; current = l}
  | `Right, {current=Pair (l, r); _} ->
    Some {parents = (false, l) :: z.parents; current = r}
  | _ -> None

let rec next_leaf ?(branched=false) origin dir z =
  let do_path to_add (v, p) = (v, to_add :: p) in
  let opp = function `Left -> `Right | `Right -> `Left | `Up -> assert false in
  let go_up () = match (move `Up z) with
    | None -> (None, [])
    | Some next ->
      let from = if fst (List.hd_exn z.parents) then `Left else `Right in
      do_path from (next_leaf ~branched from dir next)
  in
  if not branched then
    if Poly.(origin = dir) then go_up ()
    else match move dir z with
    | None -> go_up ()
    | Some next -> do_path `Up (next_leaf ~branched:true dir dir next)
  else match move (opp dir) z with
    | None -> (Some z, [])
    | Some next -> do_path `Up (next_leaf ~branched dir dir next)

let rec apply_path z = function
  | [] -> z
  | h :: t -> begin match move h z with
      | Some next -> apply_path next t
      | None -> assert false
    end

let change_and_rewind f (z, p) =
  let current = match z.current with
    | Regular n -> Regular (f n)
    | _ -> assert false
  in apply_path {z with current} (List.rev p)

let explode z = match z.current with
  | Pair (Regular a, Regular b) ->
    let z = begin match next_leaf `Left `Left z with
      | (Some x, p) -> change_and_rewind ((+) a) (x, p)
      | (None, _) -> z
    end in
    let z = begin match next_leaf `Right `Right z with
      | (Some x, p) -> change_and_rewind ((+) b) (x, p)
      | (None, _) -> z
    end in
    {z with current = Regular 0}
  | _ -> assert false

let split z = match z.current with
  | Regular n ->
    let a = n / 2 in
    let b = n / 2 + (n % 2) in
    {z with current = Pair (Regular a, Regular b)}
  | _ -> assert false

let rec magnitude (n : number) : int = match n with
  | Regular n -> n
  | Pair (l, r) -> 3 * (magnitude l) + 2 * (magnitude r)

let rec rewind z =
  if List.is_empty z.parents then z.current
  else rewind (move `Up z |> Option.value_exn)

let rec explore_explode level z =
  match z.current with
  | Pair _ when level >= 4 -> Some(explode z |> rewind)
  | Regular _ -> None
  | Pair _ -> begin
      match explore_explode (level + 1) (move `Left z |> Option.value_exn) with
      | Some x -> Some x
      | None -> explore_explode (level + 1) (move `Right z |> Option.value_exn)
    end

let rec explore_split z =
  match z.current with
  | Regular n when n >= 10 -> Some (split z |> rewind)
  | Regular _ -> None
  | Pair _ -> begin
      match explore_split  (move `Left z |> Option.value_exn) with
      | Some x -> Some x
      | None -> explore_split  (move `Right z |> Option.value_exn)
    end

let rec normalize n =
  let init = {current=n; parents=[]} in
  match explore_explode 0 init with
  | None -> begin match explore_split init with
      | None -> n
      | Some next -> normalize next
    end
  | Some next -> normalize next

let add x y = normalize (Pair (x, y))

let solve1 l = List.reduce_exn l ~f:add |> magnitude 

let solve2 l =
  let l1 = List.cartesian_product l l
  |> List.filter ~f:(fun (x,y) -> not Poly.(x = y)) in
  let l2 = List.map l1 ~f:(fun (x, y) -> (y, x)) in
  List.concat [l1; l2]
  |> List.map ~f:(fun (x,y) -> add x y |> normalize |> magnitude)
  |> List.max_elt ~compare
  |> Option.value_exn
  

let convert_data (l : string list) : number list =
  let make_number s =
    let i = ref 0 in
    let rec aux () = 
      if Char.(s.[!i] = '[') then (
        Int.incr i ;
        let l = aux () in
        assert Char.(s.[!i] = ',') ;
        Int.incr i ;
        let r = aux () in
        assert Char.(s.[!i] = ']') ;
        Int.incr i ;
        Pair (l, r)
      )
      else (
        let n = Int.of_string (String.of_char s.[!i]) in
        Int.incr i ;
        Regular n
      )
    in aux ()
  in List.map ~f:make_number l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
