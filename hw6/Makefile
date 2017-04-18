all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native:
	ocamlbuild -Is util,x86,ll,grading  main.native -use-menhir

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -r output
	rm a.out
