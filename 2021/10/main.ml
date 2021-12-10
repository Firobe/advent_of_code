open Base

let score1 = function
  | `Paren -> 3 | `Bracket -> 57 | `Brace -> 1197 | `Chevron -> 25137

let rec stack_of_line ?(stack=[]) = function
  | [] -> if List.is_empty stack then `Correct else `Incomplete stack
  | (symbol, `Open) :: t -> stack_of_line ~stack:(symbol :: stack) t
  | (symbol, `Close) :: t -> begin match stack with
      | [] -> `Corrupted symbol
      | h :: _ when Poly.(symbol <> h) -> `Corrupted symbol
      | _ :: t' -> stack_of_line ~stack:t' t
    end

let solve1 l =
  List.sum (module Int) l ~f:(fun l ->
      match stack_of_line l with
      | `Corrupted s -> score1 s
      | _ -> 0
    )

let score_sym = function
  | `Paren -> 1 | `Bracket -> 2 | `Brace -> 3 | `Chevron -> 4

let solve2 l = 
  let add_score sc sy = sc * 5 + score_sym sy in
  let scores = List.filter_map l ~f:(fun l ->
      match stack_of_line l with
      | `Incomplete s -> Some (List.fold s ~f:add_score ~init:0)
      | _ -> None
    )
  |> List.sort ~compare in
  let len = List.length scores in
  List.nth_exn scores (len / 2)

let convert_data (l : string list) =
  let conv = function
    | '(' -> (`Paren, `Open)   | ')' -> (`Paren, `Close)
    | '[' -> (`Bracket, `Open) | ']' -> (`Bracket, `Close)
    | '{' -> (`Brace, `Open)   | '}' -> (`Brace, `Close)
    | '<' -> (`Chevron, `Open) | '>' -> (`Chevron, `Close)
    | _ -> failwith "Wrong input"
  in
  List.map l ~f:(fun s -> String.to_list s |> List.map ~f:conv)

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
