type chunk = Prerelease of string | Num of int | String of string

(* Chunk ordering: Prerelease < Num < String.
   Within each kind: Num uses numeric order; Prerelease and String use
   lexicographic order, so "alpha" < "beta" < "rc". *)
let chunk_compare c1 c2 =
  match (c1, c2) with
  | Num i1, Num i2 -> Int.compare i1 i2
  | Prerelease p1, Prerelease p2 -> String.compare p1 p2
  | String s1, String s2 -> String.compare s1 s2
  | Prerelease _, Num _ -> -1
  | Prerelease _, String _ -> -1
  | Num _, Prerelease _ -> 1
  | String _, Prerelease _ -> 1
  | Num _, String _ -> -1
  | String _, Num _ -> 1

let chunk_to_string = function
  | Prerelease s -> s
  | Num n -> Int.to_string n
  | String s -> s

type t = { string : string; sortable : chunk list; major : int }

let is_digit c = c >= '0' && c <= '9'

let rec parse_string acc s =
  let n = String.length s in
  if n = 0 then List.rev acc
  else if s.[0] = '+' then List.rev acc
  else if s.[0] = '~' then begin
    let i = ref 1 in
    while !i < n && not (is_digit s.[!i]) do incr i done;
    let pre = String.sub s 1 (!i - 1) in
    parse_string (Prerelease pre :: acc) (String.sub s !i (n - !i))
  end
  else if is_digit s.[0] then begin
    let i = ref 0 in
    while !i < n && is_digit s.[!i] do incr i done;
    let num = int_of_string (String.sub s 0 !i) in
    let rest =
      if !i < n && s.[!i] = '.' then String.sub s (!i + 1) (n - !i - 1)
      else String.sub s !i (n - !i)
    in
    parse_string (Num num :: acc) rest
  end
  else begin
    let i = ref 0 in
    while !i < n && not (is_digit s.[!i]) do incr i done;
    parse_string (String (String.sub s 0 !i) :: acc) (String.sub s !i (n - !i))
  end

let of_string s : t =
  let sortable = parse_string [] s in
  let major =
    List.find_map (function Num m -> Some m | _ -> None) sortable
    |> Option.value ~default:0
  in
  { string = s; sortable; major }

let to_string_list (l : t) = List.map chunk_to_string l.sortable
let to_string t = t.string
let major t = t.major

(* Version ordering: chunks are compared left-to-right using chunk_compare.
   When one version has more chunks than the other, the tiebreak rule depends
   on the kind of the leftover chunk: a trailing Prerelease makes the version
   *smaller* (e.g. 5.0.0~alpha1 < 5.0.0), while a trailing Num or String makes
   it *larger* (e.g. 4.10 < 4.10.1).  The + build-metadata suffix is stripped
   during parsing, so +variant builds compare equal to the base version. *)
let compare v1 v2 =
  let rec inner v1 v2 =
    match (v1, v2) with
    | [], Prerelease _ :: _ -> 1
    | Prerelease _ :: _, [] -> -1
    | [], [] -> 0
    | [], Num _ :: _ -> -1
    | Num _ :: _, [] -> 1
    | [], String _ :: _ -> -1
    | String _ :: _, [] -> 1
    | h1 :: t1, h2 :: t2 -> begin
        let result = chunk_compare h1 h2 in
        if result <> 0 then result else inner t1 t2
      end
  in
  inner v1.sortable v2.sortable

let is_stable t =
  not (List.exists (function Prerelease _ -> true | _ -> false) t.sortable)

let deduplicate equal l =
  let rec loop acc = function
    | [] -> List.rev acc
    | [ x ] -> loop (x :: acc) []
    | i1 :: i2 :: tl ->
        if equal i1 i2 then loop acc (i1 :: tl) else loop (i1 :: acc) (i2 :: tl)
  in
  loop [] l

let sort_and_filter ts =
  List.sort compare ts |> List.filter is_stable
  |> deduplicate (fun v1 v2 -> compare v1 v2 = 0)
