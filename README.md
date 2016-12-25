# Supermonads for GHC

Implementation of supermonads for GHC.

## GHC Version

The implementation has been tested with GHC in version 7.10.3 and GHC 8.0.1.

Versions of GHC prior to version 7.10.1 will most certainly not work,
because the plugin mechanism was still in development.

Newer version of GHC may work. If you encounter problems with a newer version
of GHC, please file a bug report so they can be fixed.

## Usage

To use supermonads in a module you need to do the following:

* Enable `RebindableSyntax` in your module by using the `LANGUAGE` pragma:
  
  ```{-# LANGUAGE RebindableSyntax #-}```
  
* Enable the plugin in that modules using the the `OPTIONS_GHC` pragma:
  
  ```{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}```
  
* Import the supermonad prelude `Control.Supermonad.Prelude`.
  If you choose to work with constrained monads you will need to
  import `Control.Supermonad.Constrained.Prelude` instead.
* Make sure to compile the module with the `-dynamic` flag.
  This is required for GHC's plugin mechanism to work properly.

## Bug Reports

If you file a bug report, please always include the version of GHC 
you are working with and a minimal example that shows the problem.

## Examples

Examples for the use of the plugin with different kinds of monad generalizations 
are provided in the `examples` directory. All examples have their own separate 
cabal file and offer a version of the code with and without the use of supermonads.
