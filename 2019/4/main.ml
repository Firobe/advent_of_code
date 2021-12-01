let dtl x =
  let rec aux acc x =
    if x = 0 then acc
    else aux ((x mod 10) :: acc) (x / 10)
  in aux [] x

let rec increase = function
  | [] -> true
  | [_] -> true
  | a :: b :: _ when a > b -> false
  | _ :: x :: tail -> increase (x :: tail)

let adj l =
  let rec aux old l = match old, l with
  | _, [] -> false
  | _, [_] -> false
  | None, [_; _] -> true
  | Some o, [a; aa] -> o <> a && a = aa
  | None, a :: aa :: b :: _ when a <> b && a = aa -> true
  | Some c, a :: aa :: b :: _ when c <> a && b <> a && a = aa -> true
  | _, h :: t -> aux (Some h) t
  in aux None l

let correct x =
  let l = dtl x in
  (*
  Printf.printf "%d : " x;
  List.iter (Printf.printf "%d ") l;
  Printf.printf " -> %B %B\n" (increase l) (adj l);
     *)
  increase l && adj l

let rec search acc f t =
  if f > t then acc
  else
    let n = if correct f then 1 else 0 in
    (search[@tailcall]) (acc + n) (f + 1) t

let _ =
  let r = search 0 197487 673251 in
  print_int r;
  print_newline ();
