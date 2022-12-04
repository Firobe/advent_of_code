open Aoclib

module Types = struct
  type unvalidated_passport = (string * string) list [@@deriving show]
  type input = unvalidated_passport list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  (*open Parsing*)

  let whitespace = function ' ' | '\n' -> true | _ -> false

  let field =
    both (take_while1 Base.Char.is_alpha <* char ':') (take_till whitespace)

  let passport = many (field <* satisfy whitespace)
  let input = sep_by end_of_line passport
end

module Solving = struct
  open Base

  type passport = {
    birth_year : string;
    issue_year : string;
    expiration_year : string;
    height : string;
    hair_color : string;
    eye_color : string;
    passport_id : string;
    country_id : string option;
  }
  [@@warning "-69"]

  let validate1 raw =
    let get = List.Assoc.find_exn raw ~equal:String.equal in
    try
      Result.return
        {
          birth_year = get "byr";
          issue_year = get "iyr";
          expiration_year = get "eyr";
          height = get "hgt";
          hair_color = get "hcl";
          eye_color = get "ecl";
          passport_id = get "pid";
          country_id = List.Assoc.find raw ~equal:String.equal "cid";
        }
    with Not_found_s _ -> Result.fail "validate1"

  let part1 (input : input) : output =
    List.count input ~f:(Fn.compose Result.is_ok validate1)

  let validate2 pass =
    let res msg v = if v then Result.return () else Result.fail msg in
    let digits len min max t =
      let b =
        String.length t = len
        && String.for_all t ~f:Char.is_digit
        &&
        let i = Int.of_string t in
        min <= i && i <= max
      in
      res (Printf.sprintf "digits (%d %d %d) %s" len min max t) b
    in
    let height v =
      try
        match v.[3] with
        | 'c' -> digits 3 150 193 (String.sub ~pos:0 ~len:3 v)
        | 'n' -> digits 2 59 76 (String.sub ~pos:0 ~len:2 v)
        | _ -> Result.failf "height %s" v
      with _ -> Result.failf "height %s" v
    in
    let ecl = function
      | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" ->
          Result.return ()
      | x -> Result.failf "ecl %s" x
    in
    let hex v =
      (let is_ok = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
       String.length v = 7
       && Poly.(v.[0] = '#')
       && String.for_all ~f:is_ok (String.sub v ~pos:1 ~len:6))
      |> res (Printf.sprintf "hex %s" v)
    in
    let ( let* ) = Stdlib.Result.bind in
    let* () = digits 4 1920 2002 pass.birth_year in
    let* () = digits 4 2010 2020 pass.issue_year in
    let* () = digits 4 2020 2030 pass.expiration_year in
    let* () = digits 9 0 999999999 pass.passport_id in
    let* () = height pass.height in
    let* () = ecl pass.eye_color in
    hex pass.hair_color

  let part2 (input : input) : output =
    List.map input ~f:validate1
    |> List.map ~f:(Result.bind ~f:validate2)
    |> List.count ~f:(function
         | Ok () -> true
         | Error msg ->
             Stdlib.Format.printf "%s\n" msg;
             false)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
