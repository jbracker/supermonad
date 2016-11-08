 
install: init-sandbox
	cabal install

exec: install
	# INFO: Execution can take several minutes!
	time ./.cabal-sandbox/bin/ebba-example

exec-sa: install
	# INFO: Execution can take several minutes!
	time ./.cabal-sandbox/bin/ebba-example-sa

reinstall : clean-all install

clean: clean-build

clean-all: clean-build clean-gnewplot

clean-build:
	rm -f ./Ebba.o
	rm -f ./Ebba.hi
	rm -f ./Ebba
	rm -f ./*.eps
	rm -fR ./dist/

clean-sandbox:
	rm -fR ./.cabal-sandbox
	rm -f ./cabal.sandbox.config

clean-gnewplot: clean-sandbox
	rm -fR ./gnewplot

init-sandbox:
	([ -f ./cabal.sandbox.config ] && [ -d ./.cabal-sandbox ]) || cabal sandbox init
	[ -d ./gnewplot ] || (git clone https://github.com/glutamate/gnewplot.git && cabal sandbox add-source ./gnewplot)
	cabal sandbox add-source /home/jbracker/university/repos/supermonad

