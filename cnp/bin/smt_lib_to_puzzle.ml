
exception Err of string

let out = Printf.printf

type value =
  | BoolVar of bool
  | IntVar of int

let cell_regex = Str.regexp "r[0-9]+c[0-9]+$"
let region_regex = Str.regexp "r[0-9]+c[0-9]_root$"
let int_regex = Str.regexp "[0-9]+"
let negative_regex = Str.regexp "- [0-9]+"
let true_regex = Str.regexp ".*true)"
let false_regex = Str.regexp ".*false)"

let get_vars = 
  let match_bool s = 
    if Str.string_match true_regex s 0 then BoolVar(true)
    else if Str.string_match false_regex s 0 then BoolVar(false)
    else raise (Err "Internal error")
  in List.map (fun (v, x) -> let z = List.hd (List.tl v)
  in (z, (try let _ = Str.search_forward int_regex x 0 in 
    (let i = int_of_string (Str.matched_string x) in
    try let _ = Str.search_forward negative_regex x 0 in IntVar(-i) with Not_found -> IntVar(i))  
    with Not_found -> (match_bool x))))

(*
let extract regex vars = 
  let rec loop cells other = function
    | (v, x)::ls -> if Str.string_match cell_regex v 0
      then loop ((Str.matched_string v, x)::cells) other ls else loop cells ((v, x)::other) ls
    | [] -> (cells, other)
  in loop [] [] vars
*)

let extract regex vars = 
  let rec loop = function
    | (v, x)::ls -> if Str.string_match regex v 0 then
      let z = (try Str.matched_string v with (Invalid_argument _) -> "Something internal has broken") in ((z, x)::loop ls) else loop ls
    | [] -> []
  in loop vars

let get_regions (r, c) vars =
  let rec attach cell i = function
    | (index, cells)::ls -> if i = index then (i, cell::cells)::ls 
      else (index, cells)::attach cell i ls
    | [] -> [(i, [cell])]
  in let rec group regions = function
    | (cell, root)::ls -> group (attach cell root regions) ls
    | [] -> regions
  in let rec display = function
    | (_, cells)::ls -> let _ = List.map (fun c -> out "%s\n" c) cells in out "\n"; display ls
    | [] -> ()
  in out "Regions:\n"; display (group [] (List.map (fun (v, x) -> (List.hd (String.split_on_char '_' v), x)) vars))

(*
let rec get_bool = function
  | (v, x)::ls -> (match x with
    | BoolVar(_) -> (v, x)::get_bool ls
    | _ -> get_bool ls)
  | [] -> []

let rec get_int = function
  | (v, x)::ls -> (match x with
    | IntVar(_) -> (v, x)::get_bool ls
    | _ -> get_bool ls)
  | [] -> []
*)

let revert dims model = 
  let _ = List.map (fun s -> Printf.printf "%s\n" s) model in () ;
  if List.hd model = "unsat" then raise (Err "Puzzle is unsolvable")
  else let get_values s = Str.split (Str.regexp " +") s  
  in let rec unpack = function
    | f::v::ls -> (get_values f, v)::unpack ls
    | _ -> []

  in let vars = get_vars (unpack (List.tl (List.tl model)))
  in let cells = extract cell_regex vars
  in let regions = extract region_regex vars
  in get_regions dims regions
(*

  in List.map (fun (x, v) -> Printf.printf "%s" x; match v with
    | BoolVar(b) -> Printf.printf " %b\n" b
    | IntVar(i) -> Printf.printf " %i\n" i) z
  *)
    (*
  in List.map (fun (_, v) -> Printf.printf "%s\n" v) (unpack (List.tl (List.tl model)))   *)
  