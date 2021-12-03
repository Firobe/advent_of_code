open Base

let dec_of_bin l = (* I'm very ugly *)
  let chars = List.map ~f:(fun x -> if x then '1' else '0') l in
  Int.of_string @@ "0b" ^ (String.of_char_list chars)

let most_common_in_column pos l =
  let ones = List.nth_exn (List.transpose_exn l) pos |>
             List.count ~f:Fn.id in
  let zeroes = List.length l - ones in
  if ones > zeroes then `One else
  if ones = zeroes then `Equal else `Zero

let solve1 l = (* not performant but shrug *)
  let gamma = List.init (List.length (List.hd_exn l))
      ~f:(fun pos -> Poly.(most_common_in_column pos l = `One)) in
  let epsilon = List.map ~f:(Bool.(<>) true) gamma in
  (dec_of_bin gamma) * (dec_of_bin epsilon)

let filter_by_bit b pos l =
  List.filter l ~f:(fun l' -> Bool.(b = List.nth_exn l' pos))

let rec filter_process criterion ?(pos=0) candidates = 
  match candidates with
  | [] -> failwith "No candidates left"
  | [x] -> dec_of_bin x
  | _ ->
    let chosen = match criterion, most_common_in_column pos candidates with
      | `Oxygen, (`One | `Equal) | `C02, `Zero -> true
      | `Oxygen, `Zero | `C02, (`One | `Equal) -> false
    in filter_process criterion ~pos:(pos + 1)
      (filter_by_bit chosen pos candidates)

let solve2 l = (filter_process `Oxygen l) * (filter_process `C02 l)

let convert_line l = l |> String.to_list |> List.map ~f:(Char.(=) '1')

let main file =
  let input = Stdio.In_channel.read_lines file |> List.map ~f:convert_line in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file (solve1 input) (solve2 input)

let () = main "example" ; main "input"
