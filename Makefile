test_init : parser.mly lexer.mll test.ml
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c test.ml 
	ocamlc -o t parser.cmo lexer.cmo test.cmo

test : t
	./t $(file)


init : parser.mly lexer.mll main.ml
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c main.ml 
	ocamlc -o main parser.cmo lexer.cmo main.cmo
	
run : main
	./main $(file)

clean :
	rm -f *.cmo *.cmi *.cmx *.o *.mli lexer.ml parser.ml main
