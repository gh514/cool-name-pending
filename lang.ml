

let file = "test.txt"

let _ = 

  let reader = open_in file in
    try 
      let line = input_line reader in
        let result = Parser.main Lexer.token line in
          
        print_endline line;
        flush stdout;
        

        close_in reader;
    with e ->
      close_in_noerr reader;
      raise e
  