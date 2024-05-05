
include Core
include Core_unix

let file = "test.cnp"

let init_lexbuf file = 
  let program = In_channel.read_all (Printf.sprintf "./%s" file)
  in let lexbuf = Lexing.from_string (Str.replace_first (Str.regexp "Normal Sudoku Rules Apply") 
    "Box box1 = [R1C1 To R3C3];
    Box box2 = [R1C4 To R3C6];
    Box box3 = [R1C7 To R3C9];
    Box box4 = [R4C1 To R6C3];
    Box box5 = [R4C4 To R6C6];
    Box box6 = [R4C7 To R6C9];
    Box box7 = [R7C1 To R9C3];
    Box box8 = [R7C4 To R9C6];
    Box box9 = [R7C7 To R9C9];
  
    
    Cells In Rows Are Distinct;
    Cells In Columns Are Distinct;
    Cells In Boxes Are Distinct;
    
    Cells Contain Digits 1 To 9"
    program)
  in let _ = lexbuf.lex_curr_p <- { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; }
  in lexbuf

  let run cmd =
    let inp = Core_unix.open_process_in cmd in
    let r = In_channel.input_lines inp in
    In_channel.close inp; r

let _ = 
  let past_puzzle = Parser.main Lexer.token (init_lexbuf file) in
  let ast_puzzle = Past_to_ast.convert past_puzzle in
  let (dims, _) = ast_puzzle in
  let _ = Ast_to_smt_lib.translate ast_puzzle in 
  let solved = run "z3 puzzle.smt" in
  Smt_lib_to_puzzle.revert dims solved
