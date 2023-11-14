

let formula = Ast.Op(Ast.Integer(2), Ast.Add, Ast.Integer(3))

let buffer = Buffer.create 16

let bas = Buffer.add_string

let bac = Buffer.add_char

let new_line _ = bac buffer '\n'

let close_bracket _ = bac buffer ')'

let space _ = bac buffer ' '

let translate_op op = 
  match op with 
    | Ast.Add -> bas buffer "(+ "
    | Ast.Sub -> bas buffer "(- "
    | Ast.Mul -> bas buffer "(/ "
    | _ -> ()

let rec translate_expr e = 
  match e with 
    | Ast.Integer i -> bas buffer (string_of_int i)
    | Ast.Boolean b -> bas buffer (string_of_bool b)
    | Ast.Op (e1, op, e2) -> 
      translate_op op;
      translate_expr e1;
      space ();
      translate_expr e2;
      close_bracket ()
    | _ -> ()

let _ = 
  Buffer.clear buffer;
  bas buffer "(set-option :print-success false)\n";
  bas buffer "(set-logic AUFLIA)\n";
  translate_expr formula;
  new_line ();
  Buffer.output_buffer stdout buffer
    
