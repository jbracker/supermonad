
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
	rm -fR ./examples/constrained/dist

clean-sandbox:
	rm -fR ./.cabal-sandbox
	rm -f ./cabal.sandbox.config

doc: init
	cabal configure && cabal haddock --internal --executables

opendoc:
	xdg-open ./dist/doc/html/supermonad-plugin/index.html 

init:
	[ -f ./cabal.sandbox.config ] || [ -d ./.cabal-sandbox ] || cabal sandbox init

supermonad-examples: session-example session-chat-supermonad-example effect-example constrained-example hmtc-supermonad-example

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
