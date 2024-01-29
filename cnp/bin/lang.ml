

let file = "test.cnp"

let init_lexbuf file =  
  let lexbuf = Lexing.from_channel (open_in file)
  in let _ = lexbuf.lex_curr_p <- { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; }
  in lexbuf
    
let _ = 
  let result = Parser.main Lexer.token (init_lexbuf file) in
    let (past_exp, _) = Past_to_ast.convert result [] in
    Ast_to_smt_lib.translate past_exp;
