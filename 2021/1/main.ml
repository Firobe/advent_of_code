let solve l =
  let r, _ = List.fold_left (fun (sum, last) current -> match last with
      | None -> (sum, Some current)
      | Some last ->
        (sum + (if current > last then 1 else 0), Some current)
    ) (0, None) l
  in r

let () =
  Arg.read_arg "input" |> Array.to_list
  |> List.map int_of_string |> solve |> print_int
