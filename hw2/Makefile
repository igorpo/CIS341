DIRS=util,x86
DEPS=nums,unix,str

main.native:
	ocamlbuild -libs $(DEPS) -Is $(DIRS) main.native

main.byte:
	ocamlbuild -libs $(DEPS) -Is $(DIRS) main.byte

all: main.native

clean:
	rm -f *.native *.byte;
	ocamlbuild -clean