open Aoclib

module Types = struct
  type size = int [@@deriving show]
  type kind = Dir | File of size [@@deriving show { with_path = false }]

  type label = { name : string; kind : kind }
  [@@deriving show { with_path = false }]

  type action = Go_root | Go_up | Go_child of string | Discover of label list
  [@@deriving show { with_path = false }]

  type input = action list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let name = take_while (fun c -> Base.Char.is_alpha c || c = '.')

  let label =
    let dir = string "dir" *> return Dir in
    let file = integer >>| fun s -> File s in
    lift2 (fun kind name -> { name; kind }) (dir <|> file <* char ' ') name
    <* end_of_line

  let action =
    let root = (string "$ cd /" <* end_of_line) *> return Go_root in
    let up = (string "$ cd .." <* end_of_line) *> return Go_up in
    let go_child =
      string "$ cd " *> name <* end_of_line >>| fun n -> Go_child n
    in
    let discover =
      string "$ ls" *> end_of_line *> many label >>| fun l -> Discover l
    in
    choice [ root; up; go_child; discover ]

  let input = many action
end

module Solving = struct
  open Base

  type 'a tree = { label : 'a; children : 'a tree list }
  [@@deriving show { with_path = false }]

  type 'a path =
    | Root
    | Down of {
        path : 'a path;
        label : 'a;
        left_trees : 'a tree list;
        right_trees : 'a tree list;
      }
  [@@deriving show { with_path = false }]

  type zip = label tree * label path [@@deriving show { with_path = false }]

  let go_up (t, p) =
    match p with
    | Root -> None
    | Down { path; label; left_trees; right_trees } ->
        Some ({ label; children = left_trees @ [ t ] @ right_trees }, path)

  let go_child (t, p) name =
    match
      List.findi t.children ~f:(fun _ c -> String.equal c.label.name name)
    with
    | None -> failwith "No child with given name"
    | Some (n, subtree) ->
        let left_trees = List.take t.children n in
        let _, right_trees = List.split_n t.children (n + 1) in
        (subtree, Down { path = p; left_trees; right_trees; label = t.label })

  let rec go_root (t, p) =
    match go_up (t, p) with Some (t, p) -> go_root (t, p) | None -> (t, p)

  let discover (t, p) children =
    let make_node label = { label; children = [] } in
    let children = List.map children ~f:make_node in
    ({ t with children }, p)

  let tree_of_actions actions =
    let f zip = function
      | Go_root -> go_root zip
      | Go_up -> Option.value_exn @@ go_up zip
      | Go_child name -> go_child zip name
      | Discover labels -> discover zip labels
    in
    let init = ({ label = { name = "/"; kind = Dir }; children = [] }, Root) in
    let final = List.fold actions ~init ~f |> go_root in
    fst final

  let rec tree_fold ~init ~f tree =
    let init = f init tree in
    List.fold tree.children ~init ~f:(fun init t -> tree_fold ~init ~f t)

  let rec size t =
    match t.label.kind with
    | File s -> s
    | Dir -> List.sum (module Int) t.children ~f:size

  let sizes_of_input input =
    tree_of_actions input
    |> tree_fold ~init:[] ~f:(fun l t ->
           if Poly.(t.label.kind = Dir) then t :: l else l)
    |> List.map ~f:size

  let part1 (input : input) : output =
    sizes_of_input input
    |> List.filter ~f:(fun s -> s <= 100000)
    |> List.sum (module Int) ~f:Fn.id

  let part2 (input : input) : output =
    let sizes = sizes_of_input input in
    let total_used = List.max_elt ~compare sizes |> Option.value_exn in
    let current_free = 70000000 - total_used in
    let need_to_delete = 30000000 - current_free in
    List.filter ~f:(( <= ) need_to_delete) sizes
    |> List.min_elt ~compare |> Option.value_exn
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
