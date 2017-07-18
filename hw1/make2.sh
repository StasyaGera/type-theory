ocamlc -c ltype.ml lref_type.ml

ocamlyacc parser.mly
ocamllex -q lexer.mll

ocamlc -o hw1_reduction parser.mli parser.ml lexer.ml hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml

rm *.cm? parser.ml parser.mli lexer.ml
