open Base

module Seg = struct
  type t = A | B | C | D | E | F | G
  let compare = Poly.compare
  let sexp_of_t _ = sexp_of_int 0
  let of_char = function
    | 'a' -> A | 'b' -> B | 'c' -> C | 'd' -> D | 'e' -> E
    | 'f' -> F | 'g' -> G | _ -> assert false
end
module Seg_comparator = struct include Seg include Comparator.Make(Seg) end

type digit = Set.M(Seg_comparator).t

type entry = {
  sequences : digit list ; (* size 10 *)
  outputs : digit list (* size 4 *)
}

let segments : digit list = Seg.[
    [A; B; C; E; F; G];
    [C; F];
    [A; C; D; E; G];
    [A; C; D; F; G];
    [B; C; D; F];
    [A; B; D; F; G];
    [A; B; D; E; F; G];
    [A; C; F];
    [A; B; C; D; E; F; G];
    [A; B; C; D; F; G]
  ] |> List.map ~f:(Set.of_list (module Seg_comparator))

let rec permutations (lst : 'a list) : 'a list list = 
  let rec interleave x lst = 
    begin match lst with
      | [] -> [[x]]
      | hd::tl -> (x::lst) :: (List.map ~f:(fun y -> hd::y) (interleave x tl))
    end in match lst with
  | hd::tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [lst]

let translate_digit table =
  Set.map (module Seg_comparator) ~f:(Map.find_exn table)

let int_of_digit d =
  List.find_mapi segments
    ~f:(fun i s' -> if Set.equal d s' then Some i else None)

let int_of_input table d = d |> translate_digit table |> int_of_digit

let find_conversion l =
  let valid t = List.for_all l ~f:(fun d -> Option.is_some (int_of_input t d)) in
  let all = Seg.[A; B; C; D; E; F ; G] in
  permutations all |> List.map
    ~f:(Fn.compose (Map.of_alist_exn (module Seg_comparator)) (List.zip_exn all))
  |> List.find_exn ~f:valid

let read_digits (l : entry) : int list =
  let conv_table = find_conversion l.sequences in
  List.map l.outputs ~f:(int_of_input conv_table) |> List.filter_opt

let sum_map f = List.sum (module Int) ~f:(fun x -> f (read_digits x))
let count_unique = List.count ~f:(List.mem ~equal [1; 4; 7 ; 8])
let convert_int = List.foldi ~init:0 ~f:(fun n s d -> s + d * (10 ** (3 - n)))
let solve1 = sum_map count_unique
let solve2 = sum_map convert_int

let convert_data (l : string list) : entry list =
  let make_digit s =
    s |> String.to_list |> List.map ~f:Seg.of_char
    |> Set.of_list (module Seg_comparator)
  in let make_entry s = match String.split ~on:'|' s with
      | [sequences ; outputs] -> 
        let split_digits_clean s =
          String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
          |> List.map ~f:make_digit in
        {sequences = split_digits_clean sequences;
         outputs = split_digits_clean outputs}
      | _ -> assert false
  in List.map ~f:make_entry l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example_small" ; main "example" ; main "input"
