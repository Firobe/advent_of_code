open Base

type game_state = {
  p1 : int * int; (* pos, score *)
  p2 : int * int;
  playing : [`P1 | `P2];
  status : [`T0 | `T1 of int | `T2 of int]
}

let move_by ~from x = (from - 1 + x) % 10 + 1

let next_step gs next = match gs.status with
  | `T0 -> {gs with status = `T1 next}
  | `T1 x -> {gs with status = `T2 (x + next)}
  | `T2 x -> begin match gs.playing with
      | `P1 ->
        let p, s = gs.p1 in
        let p' = move_by ~from:p (x + next) in
        {gs with p1 = (p', s + p'); playing = `P2; status = `T0}
      | `P2 ->
        let p, s = gs.p2 in
        let p' = move_by ~from:p (x + next) in
        {gs with p2 = (p', s + p'); playing = `P1; status = `T0}
    end

let is_winning limit gs = snd gs.p1 >= limit || snd gs.p2 >= limit

let rec deterministic_play gs n next =
  if is_winning 1000 gs then gs, n
  else
    let gs' = next_step gs next in
    deterministic_play gs' (n + 1) ((next % 100) + 1)

let rec dirac_play mem gs =
  match Hashtbl.Poly.find mem gs with
  | Some x -> x
  | None ->
    let f = next_step gs in
    let data = 
      if is_winning 21 gs then
        if (snd gs.p1 > snd gs.p2) then (1, 0) else (0, 1)
      else
        [(f 1); f 2; f 3]
        |> List.map ~f:(dirac_play mem)
        |> List.reduce_exn ~f:(fun (w11, w21) (w12, w22) -> (w11+w12, w21+w22))
    in Hashtbl.Poly.add_exn mem ~key:gs ~data; data

let init_state (s1, s2) = {p1 = (s1, 0); p2 = (s2, 0); playing = `P1; status = `T0}

let solve1 l =
  let gs, n = deterministic_play (init_state l) 0 1 in
  Int.min (snd gs.p1) (snd gs.p2) * n

let solve2 l =
  let (w1, w2) = dirac_play (Hashtbl.Poly.create ~size:10000 ())
      (init_state l) in
  Int.max w1 w2

let convert_data (l : string list) : int * int = match l with
  | [p1; p2] ->
    let parse s =
      Caml.Scanf.sscanf s "Player %d starting position: %d" (fun x y -> (x, y))
    in
    let p1, s1 = parse p1 in
    let p2, s2 = parse p2 in
    assert (p1 = 1);
    assert (p2 = 2);
    (s1, s2)
  | _ -> failwith "Wrong input"

let main file =
  let input = Stdio.In_channel.read_lines file |> convert_data in
  Stdio.printf "%s\n========\nPart 1 %d\nPart 2 %d\n%!" file
    (solve1 input) (solve2 input)

let () = main "example" ; main "input"
