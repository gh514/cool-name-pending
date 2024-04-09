

{
  open Parser
  open Lexing 

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let var_regex = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let int_regex = ['0'-'9']+
let newline = ('\n'|'\r'|"\n\r")
let cell_regex = ['R'] int_regex ['C'] int_regex

rule token = parse
    | [' ' '\t']    {token lexbuf}
    | ("Grid")      {GRID}
    | ('X')         {CROSS}
    | ('R')         {R}
    | ('C')         {C}
    | ("Cell")      {CELL}
    | ("Region")    {REGION}
    | ("Line")      {LINE}
    | ("Int")       {INTDEC}
    | ("Bool")      {BOOLDEC}
    | ("Row")       {ROW}
    | ("Column")    {COLUMN}
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
    | ("->")        {LEFTIMP}
    | ("<-")        {RIGHTIMP}
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
    | ("Cells")     {CELLS}
    | ("Size")      {SIZE}
    | ("Length")    {LENGTH}
    | ("Adj")       {ADJACENT}
    | ("Sum")       {SUM}
    | ("To")        {TO}
    | ("On")        {MEMBER}
    | ("In")        {MEMBER}
    | int_regex     {INT (int_of_string (Lexing.lexeme lexbuf))}
    | var_regex     {VAR (Lexing.lexeme lexbuf)}
    | (newline)     {next_line lexbuf; token lexbuf}
    | eof           {EOF}
    | _ as c        {failwith (Printf.sprintf "unexpected character: %C" c)}