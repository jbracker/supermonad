
HMTC_BIN=./.cabal-sandbox/bin/hmtc-orig

HMTC_ORIG_TEST_FILES=\
	./examples/hmtc/original/MTTests/fac.tam \
	./examples/hmtc/original/MTTests/incdec.tam \
	./examples/hmtc/original/MTTests/matmult.tam \
	./examples/hmtc/original/MTTests/overloading.tam \
	./examples/hmtc/original/MTTests/records.tam \
	./examples/hmtc/original/MTTests/sort.tam \
	./examples/hmtc/original/MTTests/test1.tam \
	./examples/hmtc/original/MTTests/test2.tam \
	./examples/hmtc/original/MTTests/test3.tam \
	./examples/hmtc/original/MTTests/test4.tam \
	./examples/hmtc/original/MTTests/test6.tam

HMTC_SM_TEST_FILES=\
	./examples/hmtc/original/MTTests/fac.tam \
	./examples/hmtc/original/MTTests/incdec.tam \
	./examples/hmtc/original/MTTests/matmult.tam \
	./examples/hmtc/original/MTTests/overloading.tam \
	./examples/hmtc/original/MTTests/records.tam \
	./examples/hmtc/original/MTTests/sort.tam \
	./examples/hmtc/original/MTTests/test1.tam \
	./examples/hmtc/original/MTTests/test2.tam \
	./examples/hmtc/original/MTTests/test3.tam \
	./examples/hmtc/original/MTTests/test4.tam \
	./examples/hmtc/original/MTTests/test6.tam

install: init
	cabal install

clean: init
	cabal clean
	rm -fR ./examples/session/dist
	rm -fR ./examples/effect/dist
	rm -fR ./examples/hmtc/original/dist
	rm -fR ./examples/hmtc/supermonad/dist

doc: init
	cabal configure && cabal haddock

opendoc:
	xdg-open ./dist/doc/html/supermonad-plugin/index.html 

init:
	[ -f ./cabal.sandbox.config ] || [ -d ./.cabal-sandbox ] || cabal sandbox init

session-example: install
	cabal install ./examples/session
	
effect-example: install
	cabal install ./examples/effect

hmtc-orig-example: install
	cabal install ./examples/hmtc/original

hmtc-supermonad-example: install
	cabal install ./examples/hmtc/supermonad

%.tam: %.mt hmtc-orig-example
	$(HMTC_BIN) $<
	$(HMTC_BIN) $@
	rm $@

hmtc-orig-test-files: hmtc-orig-example $(HMTC_ORIG_TEST_FILES)

hmtc-supermonad-test-files: hmtc-supermonad-example $(HMTC_SM_TEST_FILES)
