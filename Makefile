all: ATM lab19

ATM: aTMcomponents.ml ./aTMcomponents.mli 
	ocamlbuild aTMcomponents.byte

lab19: ATM lab19.ml 
	ocamlbuild lab19.byte
	./lab19.byte

clean: 
	rm -rf _build/ *.byte
