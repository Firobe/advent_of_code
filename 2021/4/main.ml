open Base

type board = (int * bool) array array

type prob_input = {
  numbers : int list;
  boards : board list;
}

let find_and_mark v (board : board) : board =
  Array.map board ~f:(fun t -> Array.map t ~f:(fun (x, b) -> (x, b || x = v)))

let is_winner (b : board) : bool = 
  let row_full r = Array.for_all ~f:snd r in
  let some_row_full t = Array.exists t ~f:row_full in
  (some_row_full b || some_row_full (Array.transpose_exn b))

let sum_unmarked (b : board) : int =
  Array.fold b ~init:0 ~f:(fun s t ->
      Array.fold t ~init:s ~f:(fun s (x, b) -> if b then s else s + x))

let solve1 l =
  let rec play boards = function
    | [] -> failwith "No winner"
    | n :: t ->
      let boards = List.map ~f:(find_and_mark n) boards in
      begin match List.find ~f:is_winner boards with
        | Some winner_board -> (sum_unmarked winner_board) * n
        | None -> play boards t
      end
  in play l.boards l.numbers

(* same but remove boards as they win and keep latest winning score *)
let solve2 l =
  let rec play last_score boards = function
    | [] -> last_score
    | n :: t ->
      let boards = List.map ~f:(find_and_mark n) boards in
      let score = begin match List.find ~f:is_winner boards with
        | Some winner_board -> (sum_unmarked winner_board) * n
        | None -> last_score
      end in
      play score (List.filter boards ~f:(fun b -> not (is_winner b))) t
  in play 0 l.boards l.numbers

let convert_data (l : string list) : prob_input =
  let groups = List.group ~break:(fun _ -> String.is_empty) l in
  match groups with
  | [numbers] :: boards ->
    let numbers = String.split numbers ~on:',' |> List.map ~f:Int.of_string in
    (* Remove empty lines *)
    let boards = boards |> List.map ~f:(function
        | _ :: t ->
          List.map t ~f:(fun s ->
              String.split ~on:' ' s 
              |> List.filter ~f:(fun s -> not (String.is_empty s))
              |> List.map ~f:Int.of_string
              |> List.map ~f:(fun i -> (i, false))
            )
          |> Array.of_list_map ~f:Array.of_list
        | _ -> assert false)
    in {numbers; boards}
  | _ -> failwith "Wrong input"


let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file (solve1 input) (solve2 input)

let () = main "example" ; main "input"
