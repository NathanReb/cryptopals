BUILD=ocamlbuild -use-ocamlfind
ML=$(wildcard src/*.ml)
TARGET=$(addprefix _build/, $(addsuffix .cmi, $(basename $(ML))))
TEST=test_all.native

all: matasano check

default: matasano

matasano: $(TARGET)

_build/%:
	$(BUILD) $*

test: matasano
	$(BUILD) $(TEST)

check: test
	./$(TEST) -runner sequential


clean:
	rm -rf _build
	rm *.native
