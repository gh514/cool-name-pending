

let var_regex = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let int_regex = ['0'-'9']+
let newline = ('\010' | "\013\010" )
let cell_regex = ['R'] int_regex ['C'] int_regex

rule token = parse
    | ("Grid")      {GRID}
    | ("Cell")      {CELL}
    | ("Set")       {SET}
    | ("Region")    {REGION}
    | ("Line")      {LINE}
    | ('+')         {ADD}
    | ('-')         {SUB}
    | ('*')         {MUL}
    | ('/')         {DIV}
    | ("True")      {TRUE}
    | ("False")     {FALSE}
    | ("and")       {AND}
    | ("or")        {OR}
    | ("not")       {NOT}
    | ("xor")       {XOR}
    | ('=')         {EQUAL}
    | ('<')         {LT}
    | ('>')         {GT}
    | ("<=")        {LTE}
    | (">=")        {GTE}
    | ("!=")        {UNEQUAL}
    | ("<-")        {LEFTIMP}
    | ("->")        {RIGHTIMP}
    | ("<->")       {BIIMP}
    | ('(')         {LBRACK}
    | (')')         {RBRACK}
    | ('[')         {LSBRACK}
    | (']')         {RSBRACK}
    | (';')         {SEMICOLON}
    | (',')         {COMMA}
    | ("in")        {IN}
    | ("Forall")    {FORALL}
    | ("Exists")    {EXISTS}
    | ("!Forall")   {NFORALL}
    | ("!Exists")   {NEXISTS}
    | ('.')         {POINT}
    | ("cells")     {CELLS}
    | ("value")     {VALUE}
    | ("size")      {SIZE}
    | ("length")    {LENGTH}
    | ("adjacent")  {ADJACENT}
    | (' ')         {SPACE}
    | eof           {EOF}
    | int_regex     {INT (int_of_string (Lexing.lexeme lexbuf))}
    | var_regex     {VAR (Lexing.lexeme lexbuf)}
    | newline       {token lexbuf}