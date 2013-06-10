PACKAGES=ounit
BUILD_COMMAND=ocamlbuild -use-ocamlfind -pkgs $(PACKAGES)

calc:
	$(BUILD_COMMAND) calc.byte
calc.native:
	$(BUILD_COMMAND) calc.native
test:
	$(BUILD_COMMAND) unit_tests.byte --
clean:
	$(BUILD_COMMAND) -clean
