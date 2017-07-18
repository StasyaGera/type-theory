ocamlc -c ltype.ml

ocamlyacc parser.mly
ocamllex -q lexer.mll

ocamlc -o hw1 parser.mli parser.ml lexer.ml hw1.mli hw1.ml

rm *.cm? parser.ml parser.mli lexer.ml
