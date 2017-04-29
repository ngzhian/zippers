OCAMLBUILD=ocamlbuild
MAIN=test_zipper
BUILD=native

.PHONY: test

default: $(MAIN).ml zipper.ml
	$(OCAMLBUILD) $(MAIN).$(BUILD)

test: $(MAIN).$(BUILD)
	./$(MAIN).$(BUILD)

clean:
	$(OCAMLBUILD) -clean
