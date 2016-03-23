##############################################################################
#
# 	Makefile for HMTC
#	Copyright (c) Henrik Nilsson, 2006
#
##############################################################################

# Initial "all" target:
# This goes first to make it the default target.

.PHONY: all doc clean really-clean

all: hmtc


#-----------------------------------------------------------------------------
# Source files: 
#-----------------------------------------------------------------------------

# Haskell sources.
hs_sources = \
    AST.hs \
    CodeGenerator.hs \
    CodeGenMonad.hs \
    Diagnostics.hs \
    Env.hs \
    LibMT.hs \
    Main.hs \
    MTIR.hs \
    MTStdEnv.hs \
    Name.hs \
    ParseMonad.hs \
    Parser.hs \
    PPAST.hs \
    PPMTIR.hs \
    PPTAMCode.hs \
    PPUtilities.hs \
    Scanner.hs \
    ScopeLevel.hs \
    SrcPos.hs \
    Symbol.hs \
    TAMCode.hs \
    TAMCodeParser.hs \
    TAMInterpreter.hs \
    Token.hs \
    TypeChecker.hs \
    Type.hs


#-----------------------------------------------------------------------------
# Tools, arguments, and auxiliary files
#-----------------------------------------------------------------------------

SHELL = /bin/sh

HS_OPTS += -O $(HS_PACKAGES) $(HS_EXTRA_IMPORTS) $(HS_USER_OPTS)
MAKE.hs  = ghc --make $(HS_OPTS) -o $@

# ghc-specific (for now)
HAPPY = happy -agc


#-----------------------------------------------------------------------------
# Auxiliary variables
#-----------------------------------------------------------------------------

hs_interfaces := $(hs_sources:.hs=.hi)
hs_objects := $(hs_sources:.hs=.o)

#-----------------------------------------------------------------------------
# Implicit rules for Haskell
#-----------------------------------------------------------------------------

# Happy: Run CPP on the output from Happy to make Haddock happy! :-)
%.hs:   %.y
	$(HAPPY) --outfile=happy-output.hs $<
	ghc -cpp -E -optP-P -o $@ happy-output.hs
	rm happy-output.hs


#-----------------------------------------------------------------------------
# Compilation of the Haskell Mini Triangle Compiler
#-----------------------------------------------------------------------------

hmtc: $(hs_sources)
	$(MAKE.hs) Main


#-----------------------------------------------------------------------------
# Generating documentation
#-----------------------------------------------------------------------------

doc: Doc $(hs_sources)
	haddock --html --odir=Doc $(hs_sources) --title=HMTC

Doc:
	mkdir Doc


#-----------------------------------------------------------------------------
# Cleaning
#-----------------------------------------------------------------------------

clean:
	-$(RM) $(hs_interfaces) $(hs_objects) hmtc

really-clean: clean
	-$(RM) Parser.hs
	-$(RM) TAMCodeParser.hs
	-rm -rf Doc
