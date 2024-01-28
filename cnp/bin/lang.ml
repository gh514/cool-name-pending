

let file = "test.txt"

let _ = 

  let reader = open_in file in
    try 
      let lexbuf = Lexing.from_string (input_line reader) in
        let result = Parser.main Lexer.token lexbuf in
          
        print_endline "abc";
        flush stdout;
        

        close_in reader;
    with e ->
      close_in_noerr reader;
      raise e
  