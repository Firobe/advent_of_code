type tree = {data : string; next : tree list}

let rec children x = function
  | [] -> []
  | (a, b) :: t when a = x -> b :: (children x t)
  | _ :: t -> children x t

let rec build_tree l root =
  {data = root; next = List.map (build_tree l) (children root l)}

let total_depth t =
  let rec aux curd t =
      curd + List.fold_left (fun acc t' -> acc + aux (curd + 1) t') 0 t.next
  in
  aux 0 t

let rec dist_to t x =
  if t.data = x then Some 0
  else
    let rec acc = function
      | [] -> None
      | a :: b -> 
    begin match (dist_to a x) with
      | None -> acc b
      | Some d -> Some (d + 1)
    end in acc t.next

let rec search a b t =
  let minopt a b = match a, b with
    | None, None -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some x, Some y -> Some (min x y)
  in
  let cur = match (dist_to t a), (dist_to t b) with
  | Some x, Some y -> Some (x + y)
  | _ -> None
  in
  let full = List.fold_left (fun acc x -> minopt acc (search a b x)) None t.next in
  minopt full cur


let _ =
    let chan = open_in "input" in
    let l = Std.input_list chan |> 
               List.map (fun x ->
                   match Str.split (Str.regexp ")") x with
                   | [a; b] -> (a, b) | _ -> assert false) in
    let t = build_tree l "COM" in
    let r = match search "SAN" "YOU" t with Some x -> x | _ -> assert false in
    print_int r;
    print_newline ();
    close_in chan
