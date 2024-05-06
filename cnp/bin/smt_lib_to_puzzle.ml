
exception Err of string

let out = Printf.printf

type value =
  | BoolVar of bool
  | IntVar of int

let cell = "r[0-9]+c[0-9]+"
let cell_regex = Str.regexp (cell ^ "$")
let region_regex = Str.regexp (cell ^ "_root$")
let named_region_regex = Str.regexp (".*_" ^ cell ^ "$")
let line_regex = Str.regexp (".*_" ^ cell ^ "to" ^ cell) 
let nums = "[0-9]+"
let int_regex = Str.regexp nums
let negative_regex = Str.regexp ("- " ^ nums)
let true_regex = Str.regexp ".*true)"
let false_regex = Str.regexp ".*false)"

let get_end str = List.hd (List.tl (String.split_on_char '_' str))
let get_start str = List.hd (String.split_on_char '_' str)

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
      let z = (try Str.matched_string v with (Invalid_argument _) -> "Something strange has happened internally") in ((z, x)::loop ls)
      else loop ls
    | [] -> []
  in loop vars
(*
let rec get_dt v vars =
  let rec loop = function
    | (dt, Past.Var(_, var), _)::ls -> if v = var then dt else loop ls
    | [] -> raise (Err "variable not found")
  in loop vars
*)
let rec display = function
  | cell::ls -> out "%s\n" cell; display ls
  | [] -> ()

let get_regions (r, c) vars regions =
  if List.length regions > 0 then let named_regions = List.filter (fun (v, _) -> Str.string_match named_region_regex v 0) vars
    in let group_named_regions v = 
      let rec loop = function
        | (var, BoolVar(b))::ls -> Printf.printf "%s %b\n" var b; if Str.string_match (Str.regexp v) var 0 && b then 
          get_end var::loop ls else loop ls
        | [] -> []
      in loop named_regions
    in let named_region_groups = List.map (fun v -> (v, (Printf.printf "%i" (List.length (group_named_regions v)); group_named_regions v))) (regions)
    in if List.length named_region_groups > 0 then 
      (out "Regions:"; List.map (fun (v, vs) -> out "\n%s:\n" v; display vs) named_region_groups)
    else []

  else let model_regions = extract region_regex vars 
    in let rec attach cell i = function
      | (index, cells)::ls -> if i = index then (i, cell::cells)::ls 
        else (index, cells)::attach cell i ls
      | [] -> [(i, [cell])]
    in let rec group regs = function
      | (cell, root)::ls -> group (attach cell root regs) ls
      | [] -> regs
    in if (List.fold_left (fun b -> (fun (v, _) -> b || try get_end v = "root" with (Failure _) -> false)) false vars)
    then out "Regions:"; List.map (fun (v, ls) -> out "\n"; display ls) (group [] (List.map (fun (v, x) -> (get_start v, x)) model_regions))
  (*
    (List.fold_left (fun b -> (fun v -> b || (Str.string_match named_region_regex v 0 && List.mem (get_start v) regions)))
    false (let (names, _) = List.split vars in names))*)


let get_lines (r, c) vars lines = 2
(*  let line_segments = List.filter (fun (v, _) -> Str.string_match line_regex v 0) vars
  in let to_cells = List.map (fun (v, _) -> let str = get_end v in 
    let i = (String.index_from str 0 't') in
    List.map (fun s -> (let _ = Str.search_forward int_regex s 0 in int_of_string (Str.matched_string s)), 
      let _ = Str.search_backward int_regex s (String.length s) in  int_of_string (Str.matched_string s))
      [String.sub str 0 i; String.sub str (i+2) ((String.length str)-i-2)]) line_segments
  in let source = List.hd (List.filter )
  
  let rec order = function
    | 
*)

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

let revert dims model named_vars = 
  let rec sort ints bools cells regions lines boxes = function
    | (Past.Int, Past.Var(_, v), _)::ls -> sort (v::ints) bools cells regions lines boxes ls
    | (Past.Bool, Past.Var(_, v), _)::ls -> sort ints (v::bools) cells regions lines boxes ls
    | (Past.Cell, Past.Var(_, v), _)::ls -> sort ints bools (v::cells) regions lines boxes ls
    | (Past.Region, Past.Var(_, v), _)::ls -> sort ints bools cells (v::regions) lines boxes ls
    | (Past.Box, Past.Var(_, v), _)::ls -> sort ints bools cells regions lines (v::boxes) ls
    | (_, Past.Var(_, v), _)::ls -> sort ints bools cells regions (v::lines) boxes ls
    | [] -> (ints, bools, cells, regions, lines, boxes)
  in let (ints, bools, cells, regions, lines, boxes) = sort [] [] [] [] [] [] named_vars
  in let _ = List.map (fun s -> Printf.printf "%s\n" s) model in () ;
  if List.hd model = "unsat" then raise (Err "Puzzle is unsolvable")
  else let get_values s = Str.split (Str.regexp " +") s  
  in let rec unpack = function
    | f::v::ls -> (get_values f, v)::unpack ls
    | _ -> []
  in let all_vars = get_vars (unpack (List.tl (List.tl model)))
  in let cells = extract cell_regex all_vars
  in let _ = get_regions dims all_vars regions
  in let _ = get_lines dims all_vars in ()
(*

  in List.map (fun (x, v) -> Printf.printf "%s" x; match v with
    | BoolVar(b) -> Printf.printf " %b\n" b
    | IntVar(i) -> Printf.printf " %i\n" i) z
  *)
    (*
  in List.map (fun (_, v) -> Printf.printf "%s\n" v) (unpack (List.tl (List.tl model)))   *)
  