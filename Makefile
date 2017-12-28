BUILD=ocamlbuild -use-ocamlfind

LIB_ML=$(basename $(wildcard src/*.ml))
LIB=$(addprefix _build/, $(addsuffix .cmi, $(LIB_ML)) $(addsuffix .cmx, $(LIB_ML)))

TOOL_ML=$(basename $(wildcard tools/*.ml))
TOOLS=$(addprefix _build/, $(addsuffix .cmi, $(TOOL_ML)) $(addsuffix .cmx, $(TOOL_ML)))

TEST=test_all.native
CRYPTOOL=cryptool.native

all: matasano cryptool check

default: matasano

matasano: $(LIB)

tools: $(TOOLS)

_build/%:
	$(BUILD) $*

cryptool: tools
	$(BUILD) $(CRYPTOOL)

test: matasano
	$(BUILD) $(TEST)

check: test
	./$(TEST) -runner sequential


clean:
	rm -rf _build
	rm -rf *.native
