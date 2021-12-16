type operator = {id: int; packets : packet list}
and ptype = Literal of int | Operator of operator
and packet = {version: int; ptype: ptype}

let rec pp_packet fmt p =
  let pp_ptype fmt = function
    | Literal x -> Format.fprintf fmt "%d" x
    | Operator {id; packets} ->
      Format.fprintf fmt "{T%d -> [%a]}" id
        (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
           pp_packet) packets
  in
  Format.fprintf fmt "{v%d -> %a}" p.version pp_ptype p.ptype

let parse_literal b =
  let rec aux b = match%bitstring b with
    | {| l : 1; n : 4 : bitstring; r : -1 : bitstring |} when l = false ->
      ([n], r)
    | {| _ : 1; n : 4 : bitstring; r : -1 : bitstring |} ->
      let l, r = aux r in (n :: l, r)
  in
  let l, r = aux b in
  let len = List.length l * 4 in
  match%bitstring Bitstring.concat l with
  | {| n : len : int |} -> (Literal (Int64.to_int n), r)

let rec parse_operator id b = match%bitstring b with
  | {| (false) : 1; sublen : 15 : int; packets : sublen : bitstring;
        r : -1 : bitstring |} ->
    (Operator {id; packets = (parse_packet_list packets)}, r)
  | {| (true) : 1; pnum : 11 : int; r : -1 : bitstring |} ->
    let (packets, r) = parse_packet_group pnum r in (Operator {id; packets}, r)

and parse_packet_list b =
  if Bitstring.bitstring_length b = 0 then []
  else 
    let (p, r) = parse_packet b in
    p :: (parse_packet_list r)

and parse_packet_group n b =
  if n = 0 then ([], b)
  else
    let (p, r) = parse_packet b in
    let (l, r) = parse_packet_group (n - 1) r in
    (p :: l, r)

and parse_packet b = match%bitstring b with
  | {| version : 3; typeID : 3; r : -1 : bitstring |} ->
    let ptype, r = match typeID with
      | 4 -> parse_literal r
      | _ -> parse_operator typeID r
    in {version; ptype}, r
  | {| _ |} -> failwith "Wrong input"

let parse b = parse_packet b |> fst

let solve1 b =
  let rec aux p =
    let r = match p.ptype with
      | Literal _ -> 0
      | Operator {packets; _} ->
        List.fold_left (fun s p -> aux p + s) 0 packets
    in p.version + r
  in parse b |> aux

let solve2 b =
  let rec eval p = match p.ptype with
    | Literal v -> v
    | Operator {id; packets} ->
      let values = List.map eval packets in
      let binary id a b =
        let b = begin match id with
          | 5 -> a > b
          | 6 -> a < b
          | 7 -> a = b
          | _ -> failwith "Wrong operator"
        end in if b then 1 else 0
      in begin match id with
        | 0 -> List.fold_left (+) 0 values
        | 1 -> List.fold_left ( * ) 1 values
        | 2 -> Base.List.min_elt ~compare values |> Option.get
        | 3 -> Base.List.max_elt ~compare values |> Option.get
        | _ -> binary id (List.nth values 0) (List.nth values 1)
      end
  in parse b |> eval

let convert_data (l : string list) : Bitstring.t =
  let s = List.hd l in
  Hex.to_string (`Hex s) |> Bitstring.bitstring_of_string

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
