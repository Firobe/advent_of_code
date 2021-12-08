open Base

module Seg = struct
  type t = A | B | C | D | E | F | G
  let compare = Poly.compare
  let sexp_of_t _ = sexp_of_int 0
end
module Seg_comparator = struct include Seg include Comparator.Make(Seg) end

type digit = (Seg.t, Seg_comparator.comparator_witness) Set.t
type entry = {
  sequences : digit list ; (* size 10 *)
  outputs : digit list (* size 4 *)
}

let solve1 l =
  let has_unique_length s = List.mem [2; 4 ; 3 ; 7] (Set.length s) ~equal in
  List.sum (module Int) l ~f:(fun e -> List.count e.outputs ~f:has_unique_length)

let segments : (int * digit) list = Seg.[
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
  ] |> List.map ~f:(Set.of_list (module Seg_comparator)) |> List.mapi ~f:(fun i s -> (i, s))

let rec permutations (lst : 'a list) : 'a list list = 
  let rec interleave x lst = 
    begin match lst with
      | [] -> [[x]]
      | hd::tl -> (x::lst) :: (List.map ~f:(fun y -> hd::y) (interleave x tl))
    end in match lst with
  | hd::tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [lst]

let can_match table (input : digit) (_, (translation : digit)) : bool =
  let translation_l = Set.to_list translation in
  Set.to_list input |> List.map ~f:(Map.find_exn table) |> permutations
  |> List.exists ~f: (List.for_all2_exn translation_l ~f:(Fn.flip Set.mem))

let discriminate table (s : digit) : int option =
  let cand_len = List.filter segments ~f:(fun (_, s') -> Set.length s = Set.length s') in
  match cand_len |> List.filter ~f:(can_match table s) with
  | [(i, _)] -> Some i
  | [] -> failwith "No candidate"
  | _ -> None

let add_constraint table (ds : digit) (d : int) =
  Map.mapi table ~f:(fun ~key ~data ->
      let segs = Caml.ListLabels.assoc d segments in
      if Set.mem ds key then Set.inter data segs
      else Set.diff data segs
    )

let try_conv table (l : digit list) =
  match List.find_map l ~f:(fun ds ->
      Option.map ~f:(fun d -> (ds, d)) (discriminate table ds)) with
  | Some (ds, d) ->
    let new_table = add_constraint table ds d in
    let left = List.filter l ~f:(Fn.non (Set.equal ds)) in
    left, new_table
  | None -> failwith "Cannot progress"

let read_output (l : entry) : int =
  let init_table =
    let s = Set.of_list (module Seg_comparator) Seg.[A;B;C;D;E;F;G] in
    Map.of_alist_exn (module Seg_comparator)
      [(A,s);(B,s);(C,s);(D,s);(E,s);(F,s);(G,s)]
  in 
  let rec conv_until_done table sequences  =
    let ambiguous, table = try_conv table sequences in
    if List.is_empty ambiguous then table
    else conv_until_done table ambiguous
  in
  let conv_table = conv_until_done init_table l.sequences in
  let digits = List.filter_map l.outputs ~f:(discriminate conv_table) in
  List.foldi digits ~init:0 ~f:(fun n s d -> s + d * (10 ** (3 - n)))

let solve2 l = List.sum (module Int) ~f:read_output l

let seg_of_char = let open Seg in function
  | 'a' -> A | 'b' -> B | 'c' -> C | 'd' -> D | 'e' -> E
  | 'f' -> F | 'g' -> G | _ -> assert false

let convert_data (l : string list) : entry list =
  let make_digit s =
    s |> String.to_list |> List.map ~f:seg_of_char
    |> Set.of_list (module Seg_comparator)
  in let make_entry s = match String.split ~on:'|' s with
      | [sequences ; outputs] -> 
        let split_digits_clean s =
          String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
          |> List.map ~f:make_digit
        in
        let sequences = split_digits_clean sequences in
        let outputs = split_digits_clean outputs in
        {sequences; outputs}
      | _ -> assert false
  in List.map ~f:make_entry l

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n" file
    (solve1 input) (solve2 input)

let () = main "example_small" ; main "example" ; main "input"
