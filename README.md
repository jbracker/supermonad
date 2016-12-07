# Supermonads for GHC

Implementation of supermonads for GHC. See the 
"Supermonads: One Notion to Bind Them All" paper for detailed explanation
of version 0.1.

The library offers two definitions for supermonads. The version 
in `Control.Super.Monad` does not have support for constrained
monads. The version in `Control.Super.Monad.Constrained` has 
support for constrained monads. The different versions are 
provided, because working with the additional constraints 
required for constrained monads can be cumbersome sometimes.
We want to offer people a possibility to experiment with supermonads 
without having to bother with these constraints, if they don't need 
constrained monads. If you have any feedback or suggestions for 
improvement from you usage of supermonads, please leave them on
the GitHub bug tracker or write an email to the maintainer.
These parallel structures will not be maintained indefinitely and
at some point we will probably only offer supermonads with support
for constrained monads.

* master: [![build status master][TravisBuildMaster]](https://travis-ci.org/jbracker/supermonad)
* dev: [![build status dev][TravisBuildDev]](https://travis-ci.org/jbracker/supermonad)

## GHC Version

The implementation has been tested with GHC in version 7.10.3 and GHC 8.0.2.

Versions of GHC prior to version 7.10.1 will most certainly not work,
because the plugin mechanism was still in development.

Newer version of GHC may work. If you encounter problems with a newer version
of GHC, please file a bug report so they can be fixed.

## Usage

To use supermonads in a module you need to do the following:

* Enable `RebindableSyntax` in your module by using the `LANGUAGE` pragma:
  
  ```{-# LANGUAGE RebindableSyntax #-}```
  
* Enable the plugin in that modules using the the `OPTIONS_GHC` pragma:
  
  ```{-# OPTIONS_GHC -fplugin Control.Super.Monad.Plugin #-}```
  
* Import the supermonad prelude `Control.Super.Monad.Prelude`.
  If you choose to work with constrained monads you will need to
  import `Control.Super.Monad.Constrained.Prelude` instead.
* Make sure to compile the module with the `-dynamic` flag.
  This is required for GHC's plugin mechanism to work properly.

## Bug Reports

If you file a bug report, please always include the version of GHC 
you are working with and a minimal example that shows the problem.

## Examples

Examples for the use of the plugin with different kinds of monad generalizations 
are provided in the `examples` directory. All examples have their own separate 
cabal file and offer a version of the code with and without the use of supermonads.

## Known Problems

None at the moment.


[TravisBuildMaster]: https://travis-ci.org/jbracker/supermonad.svg?branch=master
[TravisBuildDev]: https://travis-ci.org/jbracker/supermonad.svg?branch=dev