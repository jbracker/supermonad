
DIR:=$(shell pwd)

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
	rm -fR ./examples/ebba/dist
	cabal sandbox delete-source $(DIR)/examples/ebba/gnewplot
	rm -fR ./examples/ebba/gnewplot
	rm -f  ./examples/ebba/*.eps
	rm -fR ./dist

clean-sandbox:
	rm -fR ./.cabal-sandbox
	rm -f ./cabal.sandbox.config

doc: init
	cabal configure && cabal haddock --internal --executables

opendoc:
	xdg-open ./dist/doc/html/supermonad-plugin/index.html 

init:
	[ -f ./cabal.sandbox.config ] || [ -d ./.cabal-sandbox ] || cabal sandbox init

supermonad-examples: install minimal-example session-example session-chat-supermonad-example effect-example constrained-example hmtc-supermonad-example ebba-example

minimal-example: install
	cabal install ./examples/minimal

session-example: install
	cabal install ./examples/session

session-chat-orig-example: init
	cabal install ./examples/session-chat/original

session-chat-supermonad-example: init
	cabal install ./examples/session-chat/supermonad

effect-example: install
	cabal install ./examples/effect

constrained-example: install
	cabal install ./examples/constrained

hmtc-orig-example: init
	cabal install ./examples/hmtc/original

hmtc-supermonad-example: install
	cabal install ./examples/hmtc/supermonad
	
hmtc-monad-param-example: init
	cabal install ./examples/hmtc/monad-param

ebba-example: install
	[ -d ./examples/ebba/gnewplot ] || ( \
		git clone https://github.com/glutamate/gnewplot.git ./examples/ebba/gnewplot && \
		cabal sandbox add-source ./examples/ebba/gnewplot )
	cabal install ./examples/ebba




