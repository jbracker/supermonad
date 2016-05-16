
HMTC_BIN=./.cabal-sandbox/bin/hmtc-orig

HMTC_TEST_FILES=\
	./examples/hmtc/test-files/fac.tam \
	./examples/hmtc/test-files/incdec.tam \
	./examples/hmtc/test-files/matmult.tam \
	./examples/hmtc/test-files/overloading.tam \
	./examples/hmtc/test-files/records.tam \
	./examples/hmtc/test-files/sort.tam \
	./examples/hmtc/test-files/test1.tam \
	./examples/hmtc/test-files/test2.tam \
	./examples/hmtc/test-files/test3.tam \
	./examples/hmtc/test-files/test4.tam \
	./examples/hmtc/test-files/test6.tam

install: init
	cabal install

clean: init
	cabal clean
	rm -fR ./examples/session/dist
	rm -fR ./examples/session-chat/original/dist
	rm -fR ./examples/session-chat/supermonad/dist
	rm -fR ./examples/effect/dist
	rm -fR ./examples/hmtc/original/dist
	rm -fR ./examples/hmtc/supermonad/dist
	rm -fR ./examples/hmtc/monad-param/dist

doc: init
	cabal configure && cabal haddock --internal --executables

opendoc:
	xdg-open ./dist/doc/html/supermonad-plugin/index.html 

init:
	[ -f ./cabal.sandbox.config ] || [ -d ./.cabal-sandbox ] || cabal sandbox init

session-example: install
	cabal install ./examples/session

session-chat-orig-example:
	cabal install ./examples/session-chat/original

session-chat-supermonad-example:
	cabal install ./examples/session-chat/supermonad

effect-example: install
	cabal install ./examples/effect

constrained-example: install
	cabal install ./examples/constrained

hmtc-orig-example:
	cabal install ./examples/hmtc/original

hmtc-supermonad-example: install
	cabal install ./examples/hmtc/supermonad
	
hmtc-monad-param-example:
	cabal install ./examples/hmtc/monad-param

%.tam: %.mt hmtc-orig-example
	$(HMTC_BIN) $<
	$(HMTC_BIN) $@
	rm $@

hmtc-orig-test-files: hmtc-orig-example $(HMTC_TEST_FILES)

hmtc-supermonad-test-files: hmtc-supermonad-example $(HMTC_TEST_FILES)
