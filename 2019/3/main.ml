type seg =
  | H of int * (int * int)
  | V of int * (int * int)

let segments l =
  let rec aux acc (x, y) = function
    | [] -> acc
    | (dir, len) :: tail ->
      let seg = match dir with
        | 'R' -> H (y, (x, x + len))
        | 'L' -> H (y, (x, x - len))
        | 'D' -> V (x, (y, y + len))
        | 'U' -> V (x, (y, y - len))
        | _ -> assert false
      in
      let nx = match seg with V _ -> x | H (_, (_, nx)) -> nx in
      let ny = match seg with H _ -> y | V (_, (_, ny)) -> ny in
      aux (seg::acc) (nx, ny) tail
  in
  aux [] (0,0) l

let parse str =
  let s = String.sub str 1 (String.length str - 1) in
  (str.[0], int_of_string s)

let cartesian l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let rec interval_intersection (a, b) (c, d) =
  if a > b then interval_intersection (b, a) (c, d)
  else if c > d then interval_intersection (a, b) (d, c)
  else if a > c then interval_intersection (c, d) (a, b)
  else if c > b then None
  else if d > b then Some (c, b)
  else Some (c, d)

let rec rtl a b =
  if a = b then [a]
  else a :: (rtl (a + 1) b)

let isin x (a, b) = a <= x && x <= b

let inter_hv (ya, ia) (xb, ib) =
  if isin ya ib && isin xb ia then Some (xb, ya)
  else None

let inter_same (ya, (xa1, xa2)) (yb, (xb1, xb2)) f =
  if ya <> yb then []
    else match interval_intersection (xa1, xa2) (xb1, xb2) with
      | None -> []
      | Some (i1, i2) -> rtl i1 i2 |> List.map f

let test_inter acc = function
  | H (a, b), H (c, d) -> (inter_same (a, b) (c, d) (fun x -> (x, a))) @ acc
  | V (a, b), V (c, d) -> (inter_same (a, b) (c, d) (fun y -> (a, y))) @ acc
  | H (h1, h2), V (v1, v2)
  | V (v1, v2), H (h1, h2) ->
    begin match inter_hv (h1, h2) (v1, v2) with
    | None -> acc
    | Some s -> s :: acc
    end

let man (x, y) = abs x + abs y
let sd s1 s2 = compare (man s1) (man s2)

let _ =
  let chan = open_in "input" in
  match Std.input_list chan
        |> List.map ( Str.split (Str.regexp ","))
        |> List.map (List.map parse) with
  | [w1;w2] ->
    let s1 = segments w1 in
    let s2 = segments w2 in
    let candidates = cartesian s1 s2 in
    let intersections = List.fold_left test_inter [] candidates
                        |> List.filter (fun x -> x <> (0,0)) in
    let sorted = List.sort sd intersections in
    let closest = List.hd sorted in
    print_int (man closest);
    print_newline ();
    close_in chan
  | _ -> assert false
