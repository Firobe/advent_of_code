open Base

let one_day l =
  let zeroes = Queue.dequeue_exn l in
  Queue.enqueue l zeroes ;
  Queue.set l 6 Z.(Queue.get l 6 + zeroes) ;
  l

let rec n_days l n =
  if n = 0 then l
  else n_days (one_day l) (n - 1)

let count_all l = Queue.fold l ~init:Z.zero ~f:Z.add

let solve1 l = (n_days (Queue.copy l) 80) |> count_all
let solve2 l = (n_days (Queue.copy l) 256) |> count_all

let convert_data (l : string list) : Z.t Queue.t =
  let l = List.hd_exn l |> String.split ~on:',' |> List.map ~f:Int.of_string in
  List.init 9 ~f:(fun i -> List.count l ~f:((=) i))
  |> List.map ~f:Z.of_int |> Queue.of_list

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %a\nPart 2 %a\n" file
    Z.output (solve1 input) Z.output (solve2 input)

let () = main "example" ; main "input"
