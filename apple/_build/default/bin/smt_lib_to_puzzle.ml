
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
let edgeline_regex = Str.regexp ".*_r[0-9]+.5c[0-9]+.5tor[0-9]+.5c[0-9]+.5"
let source_regex = Str.regexp ".*_source$"
let sink_regex = Str.regexp ".*_sink$"
let int_regex = Str.regexp "[0-9]+"
let corner_regex = Str.regexp "[0-9]+.5"
let negative_regex = Str.regexp "- [0-9]+"
let true_regex = Str.regexp ".*true)"
let false_regex = Str.regexp ".*false)"

let get_end str = List.hd (List.tl (String.split_on_char '_' str))
let get_start str = List.hd (String.split_on_char '_' str)

let get_vars = 
  let match_bool s = 
    if Str.string_match true_regex s 0 then BoolVar(true)
    else if Str.string_match false_regex s 0 then BoolVar(false)
    else raise (Err s)
  in List.map (fun (v, x) -> let z = List.hd (List.tl v)
  in (z, (try let _ = Str.search_forward int_regex x 0 in 
    (let i = int_of_string (Str.matched_string x) in
    try let _ = Str.search_forward negative_regex x 0 in IntVar(-i) with Not_found -> IntVar(i))  
    with Not_found -> (match_bool x))))

let extract regex vars = 
  let rec loop = function
    | (v, x)::ls -> if Str.string_match regex v 0 then
      let z = (try Str.matched_string v with (Invalid_argument _) -> "Something strange has happened internally") in ((z, x)::loop ls)
      else loop ls
    | [] -> []
  in loop vars

let reorder (row, col) vars = 
  let indexed_vars = List.map (fun (rc, x) -> 
    ((let _ = Str.search_forward int_regex rc 0 in let z = (Str.matched_string rc) in int_of_string z,
    let _ = Str.search_backward int_regex rc ((String.length rc)) in int_of_string (Str.matched_string rc)), (rc, x))) vars
  in let rec find r1 c1 past z = match z with
    | ((r2, c2), v)::ls -> (if r1 = r2 && c2 = c1 + 1 then (v::(find r1 (c1 + 1) [] (past@ls)))
      else if c1 = col then find (r1+1) 0 [] (past@z)
      else find r1 c1 (((r2, c2), v)::past) ls)
    | [] -> if List.length past = 0 then [] else find (r1) (c1 + 1) [] past
  in find 1 0 [] indexed_vars

let get_regions (r, c) vars regions =
  let rec display = function
    | cell::ls -> out "%s\n" cell; display ls
    | [] -> ()
  in if List.length regions > 0 then let named_regions = List.filter (fun (v, _) -> Str.string_match named_region_regex v 0) vars
    in let group_named_regions v = 
      let rec loop = function
        | (var, BoolVar(b))::ls -> if Str.string_match (Str.regexp v) var 0 && b then 
          get_end var::loop ls else loop ls
        | [] -> []
      in loop named_regions
    in let named_region_groups = List.map (fun v -> (v, group_named_regions v)) (regions)
    in if List.length named_region_groups > 0 then 
      (out "Regions:"; List.map (fun (v, vs) -> out "\n%s:\n" v; display vs) 
      (List.map (fun (r, l) -> (r, List.map (fun (x, _) -> x) (reorder (r, c) (List.map (fun x -> (x, ())) l)))) named_region_groups))
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
    then out "Regions:"; List.map (fun (v, ls) -> out "\n"; display ls) (group [] (List.map (fun (v, x) -> (get_start v, x)) (reorder (r, c) model_regions)))

let get_lines (r, c) vars lines = 
  let rec store (v, x) vs =
    let rec loop = function
      | (var, ls)::rest -> if Str.string_match (Str.regexp (var ^ "_.+")) v 0 then (var, (v, x)::ls)::rest
        else (var, ls)::loop rest
      | [] -> let s = List.hd (String.split_on_char '_' v) in if List.mem s lines 
        then [(s, [(v, x)])]
        else []
    in loop vs

  in let rec split_vars groups = function
    | var::ls -> split_vars (store var groups) ls
    | [] -> groups

  in let handle_edges s = 
    let l = String.split_on_char '.' s
    in if List.length l = 1 then s else List.nth l ((List.length l)-2)

  in let print_line vs = 
    if List.length lines > 0 then
        let line_segments = List.filter (fun (v, _) -> Str.string_match line_regex v 0 || Str.string_match edgeline_regex v 0) vs
      in let extract_rc str = 
          let i = (String.index_from str 0 't') 
          in let separate s = ((let _ = Str.search_forward int_regex s 0 in int_of_string (Str.matched_string s)), 
            let num = handle_edges s in
            let _ = Str.search_backward int_regex num ((String.length num)) in int_of_string (Str.matched_string num))
          in (separate (String.sub str 0 i), separate (String.sub str (i+2) ((String.length str)-i-2)))
        in let to_cells = List.filter_map (fun (v, BoolVar(b)) -> if b then Some (extract_rc (get_end v)) else None) line_segments
      in
        let source = List.hd (List.tl (String.split_on_char '_' 
          (List.hd (List.filter_map (fun (v, b) -> 
            if Str.string_match source_regex v 0 then (match b with
              | BoolVar(b) -> (if b then Some v else None)
              | _ -> None)
            else None) vs))))
          
        in let source_rc = (let _ = Str.search_forward int_regex source 0 in let z = (Str.matched_string source) in int_of_string z,
          let num = handle_edges source in
          let _ = Str.search_backward int_regex num ((String.length num)) in  int_of_string (Str.matched_string num))
        in let rec order line segments =
          let rec loop = function
            | (p, c)::ls -> if p = List.hd line then order (c::line) (List.filter (fun (sp, sc) -> sp != List.hd line) segments)
              else loop ls
            | [] -> line
          in loop segments
        in let display offset list= 
          let rec loop = function
          | (r, c)::rc2::ls -> Printf.printf "r%i%sc%i%s To " r offset c offset; loop (rc2::ls)
          | [(r, c)] -> Printf.printf "r%i%sc%i%s" r offset c offset
          | _ -> raise (Err "Error printing line")
        in loop list
      in if List.fold_left (fun b -> fun (v, _) -> b || Str.string_match sink_regex v 0) false vs then
        let rec bifurcate side1 side2 found = function
          | (rc1, rc2)::ls -> if rc1 = source_rc then 
              if found then bifurcate ((rc1, rc2)::(side1@ls)) (side2@ls) found []
              else bifurcate side1 ((rc1, rc2)::side2) true ls
            else bifurcate ((rc1, rc2)::side1) ((rc1, rc2)::side2) found ls
          | [] -> (side1, side2)
        in let make_loop side1 side2 = List.rev(List.tl side1)@side2
        in display (try let t  = Str.search_forward (Str.regexp "\.") source 0 in ".5" with Not_found _ -> "")
          
        (List.rev (let (side1, side2) = bifurcate [] [] false to_cells in make_loop (order [source_rc] side1) (order [source_rc] side2))); Printf.printf "\n"
      
      else display (try let t  = Str.search_forward (Str.regexp "\.") source 0 in ".5" with Not_found _ -> "")
       (List.rev (order [source_rc] to_cells)); Printf.printf "\n"
    else ()
  in List.map (fun (v, ls) -> Printf.printf "Line %s:\n" v; print_line ls) (split_vars [] vars)

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
  in (*let _ = List.map (fun s -> Printf.printf "%s\n" s) model in Printf.printf "\n";*)
  if List.hd model = "unsat" then raise (Err "Puzzle is unsolvable")
  else let get_values s = Str.split (Str.regexp " +") s  
  in let rec unpack = function
    | f::v::ls -> (get_values f, v)::unpack ls
    | _ -> []
  in let all_vars = get_vars (unpack (List.tl (List.tl model)))
  in let cells = extract cell_regex all_vars
  in let _ = List.map (fun (v, IntVar(x)) -> Printf.printf "%s = %i\n" v x) (reorder dims cells)
  in Printf.printf "\n"; let _ = get_regions dims all_vars regions
  in let _ = get_lines dims all_vars lines 
  in let _ = List.map (fun v -> Printf.printf "%s = %i\n" v (let IntVar(i) = List.assoc v all_vars in i)) ints
  in List.map (fun v -> Printf.printf "%s = %b\n" v (let BoolVar(b) = List.assoc v all_vars in b)) bools
  
  