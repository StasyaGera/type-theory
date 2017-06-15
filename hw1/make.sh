ocamlc -c ltype.ml

ocamlyacc parser.mly
ocamllex lexer.mll

ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml

ocamlc -c hw1.mli
ocamlc -c hw1.ml

ocamlc -o hw1 lexer.cmo parser.cmo hw1.cmo

rm *.cm? parser.ml parser.mli lexer.ml
