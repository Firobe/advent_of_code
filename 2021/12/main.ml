open Base

type node = {name : string ; big : bool}
type graph = {nodes : node list ; edges : (string * string) list}
type path = string list
let add_edge g n1 n2 =
  let nodes =
    n1 :: n2 :: g.nodes
    |> List.dedup_and_sort ~compare:(fun x y -> String.compare x.name y.name)
  in let edges = (n1.name, n2.name) :: g.edges in
  {nodes; edges}
let find_node g i = List.find_exn g.nodes ~f:(fun n -> String.(n.name = i))

(* not performant at all, adj matrix if needed *)
let neighbours g n =
  List.filter_map g.edges ~f:(fun (i1, i2) ->
      if String.(i1 = n.name) then Some (find_node g i2)
      else if String.(i2 = n.name) then Some (find_node g i1)
      else None)

let rec all_paths g ?(joker=true) ?(visited=[]) src dst : path list =
  if String.(src.name = dst.name) then [[src.name]] else
    let visited = if src.big then visited else src.name :: visited in
    let process joker n =
      all_paths g ~joker ~visited n dst
      |> List.map ~f:(List.cons n.name)
    in neighbours g src
       |> List.filter_map
         ~f:(fun n ->
             if List.exists visited ~f:(fun i -> String.(i = n.name)) then
               if String.(n.name <> "start") && joker then
                 process false n |> Option.some
               else None
             else process joker n |> Option.some)
       |> List.concat

let solve joker g =
  all_paths ~joker g (find_node g "start") (find_node g "end") |> List.length
let solve1 = solve false
let solve2 = solve true

let convert_data (l : string list) =
  let is_uppercase s = String.(s = uppercase s) in
  let make_node s = {name = s; big = is_uppercase s} in
  List.fold l ~init:{nodes=[]; edges=[]}
    ~f:(fun g s -> match String.split ~on:'-' s with
        | [x; y] -> add_edge g (make_node x) (make_node y)
        | _ -> failwith "Wrong input")

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
