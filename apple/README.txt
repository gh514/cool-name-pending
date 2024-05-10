The project is executed by running "dune exec apple" in a terminal from the "apple" directory, the file puzzle.apl is then translated and solved.
Results of the translation process which are in SMT-LIB are stored in translated.smt, in you are curious.
The library "core_unix" may need to be installed which can be done with "opam install core_unix".
An installation of the z3 theorem prover is required to the project to run, which can be installed from https://github.com/Z3Prover/z3/blob/master/
The parser and lexer have been compiled already but can be compiled again with "ocamlyacc parser.mly" and "ocamllex lexer.mll" respectively, these must be run from the "bin" directory
"Examples" contains 4 example puzzles