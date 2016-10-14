# Supermonads package changelog

## 0.2

* Introduced super-applicatives through the `Applicative` class.
  This is a natural broadening of our approach.
* Renamed the constrained `CFunctor` to `Functor` so it can act as a 
  drop in replacement for the standard functor type class.
* Fixed effect monad examples by using the new version of the `effect-monad`
  package that supports GHC 8+.
* Generalized and fixed issue with the constrained `WrappedMonad` instances.

## 0.1

* Initial release.