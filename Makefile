
DIR:=$(shell pwd)

install: init
	cabal install

clean: init
	cabal clean
	
	rm -fR ./examples/test/missing-functions/dist
	
	rm -fR ./examples/monad/session/dist
	rm -fR ./examples/monad/effect/dist
	rm -fR ./examples/monad/constrained/dist
	rm -fR ./examples/monad/minimal/dist
	
	rm -fR ./examples/monad/session-chat/original/dist
	rm -fR ./examples/monad/session-chat/supermonad/dist
	
	rm -fR ./examples/monad/hmtc/original/dist
	rm -fR ./examples/monad/hmtc/supermonad/dist
	rm -fR ./examples/monad/hmtc/monad-param/dist
	
	rm -fR ./examples/applicative/ll1-parser/dist
	
	rm -fR ./dist
	rm -f  ./*.eps

clean-sandbox:
	rm -fR ./.cabal-sandbox
	rm -f ./cabal.sandbox.config

test: init
	cabal install --only-dependencies --enable-tests
	cabal test

doc: init
	cabal configure && cabal haddock --internal --executables

opendoc:
	xdg-open ./dist/doc/html/supermonad/index.html 

init:
	[ -f ./cabal.sandbox.config ] || [ -d ./.cabal-sandbox ] || cabal sandbox init

hlint:
	find src/ -iname '*.hs' -exec hlint --hint=.hlint.hs {} \;


missing-function-test:
	cabal install ./examples/test/missing-functions

examples: non-super-examples supermonad-examples superapplicative-examples

non-super-examples: install session-chat-orig-example hmtc-orig-example hmtc-monad-param-example

supermonad-examples: install minimal-example session-example session-chat-supermonad-example effect-example constrained-example hmtc-supermonad-example

superapplicative-examples: install ll1-parser-example bankers-example

# Compilation of (super)monad examples.
minimal-example: install
	cabal install ./examples/monad/minimal

session-example: install
	cabal install ./examples/monad/session

session-chat-orig-example: init
	cabal install ./examples/monad/session-chat/original

session-chat-supermonad-example: init
	cabal install ./examples/monad/session-chat/supermonad

effect-example: install
	cabal install ./examples/monad/effect

constrained-example: install
	cabal install ./examples/monad/constrained

hmtc-orig-example: init
	cabal install ./examples/monad/hmtc/original

hmtc-supermonad-example: install
	cabal install ./examples/monad/hmtc/supermonad
	
hmtc-monad-param-example: init
	cabal install ./examples/monad/hmtc/monad-param

# Compilation of (super)applicative examples.
ll1-parser-example: install
	cabal install ./examples/applicative/ll1-parser

bankers-example: install
	cabal install ./examples/applicative/bankers

