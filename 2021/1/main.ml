let solve1 l =
  let r, _ = List.fold_left (fun (sum, last) current -> match last with
      | None -> (sum, Some current)
      | Some last ->
        (sum + (if current > last then 1 else 0), Some current)
    ) (0, None) l
  in r

(* Only compute sliding windows, and pass result to solve1 *)
let solve2 l =
  (* Current sum and last three elements *)
  let rec aux sum (a, b, c) = function
    | [] -> [sum]
    | d :: t -> sum :: (aux (sum + d - a) (b, c, d) t)
  in match l with
  | a :: b :: c :: tail ->
    aux (a + b + c) (a, b, c) tail |> solve1
  | _ -> assert false


let main file =
  let input = Arg.read_arg file |> Array.to_list |> List.map int_of_string in
  Printf.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file (solve1 input) (solve2 input)

let () =
  main "example" ; main "input"
