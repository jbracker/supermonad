

install: init
	cabal install

clean: init
	cabal clean
	rm -fR ./examples/session/dist
	rm -fR ./examples/effect/dist

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
