
{-# LANGUAGE NoImplicitPrelude #-}

-- | A replacement of the standard "Prelude" for supermonads. Should provide
--   all of the functions also provided in the original prelude without
--   the functions related specifically to 'P.Monad's. The functions related
--   to 'P.Monad's are replaced with their supermonad counterparts.
--
--   A replacement for the functions in "Control.Monad" can be found 
--   in "Control.Supermonad.Functions".
module Control.Supermonad.Prelude
  ( -- * Prelude
    -- ** Standard types, classes and related functions
    -- *** Basic data types
    P.Bool(..)
  , (P.&&), (P.||), P.not, P.otherwise
  , P.Maybe(..)
  , P.maybe
  , P.Either(..)
  , P.either
  , P.Ordering(..)
  , P.Char, P.String
    -- **** Tuples
  , P.fst, P.snd, P.curry, P.uncurry
    -- *** Basic type classes
  , P.Eq(..), P.Ord(..), P.Enum(..), P.Bounded(..)
    -- *** Numbers
    -- **** Numeric types
  , P.Int, P.Integer, P.Float, P.Double, P.Rational, P.Word
    -- **** Numeric type classes
  , P.Num(..), P.Real(..), P.Integral(..)
  , P.Fractional(..), P.Floating(..)
  , P.RealFrac(..), P.RealFloat(..)
    -- **** Numeric functions
  , P.subtract, P.even, P.odd, P.gcd, P.lcm
  , (P.^), (P.^^)
  , P.fromIntegral, P.realToFrac
    -- *** Monoids
  , P.Monoid(..)
    -- *** (Monads and) functors
  , P.Functor(..), (P.<$>)
  , P.Applicative(..)
  -- , P.Monad(..) -- Nope not this one!
    -- *** Folds and traversals
  , P.Foldable(..)
  , P.Traversable(traverse, sequenceA) -- Only export the non-monad functions.
    -- *** Miscellaneous functions
  , P.id, P.const, P.flip, P.until, P.asTypeOf, P.error, P.undefined, P.seq
  , (P..), (P.$), (P.$!)
    -- ** List operations
  , P.map, P.filter, P.head, P.last, P.tail, P.init, P.reverse
  , (P.++), (P.!!)
    -- *** Special folds
  , P.and, P.or, P.any, P.all
  , P.concat, P.concatMap
    -- *** Building lists
    -- **** Scans
  , P.scanl, P.scanl1, P.scanr, P.scanr1
    -- **** Infinite lists
  , P.iterate, P.repeat, P.replicate, P.cycle
    -- *** Sublists
  , P.take, P.drop, P.splitAt, P.takeWhile, P.dropWhile, P.span, P.break
    -- *** Searching lists
  , P.notElem, P.lookup
    -- *** Zipping and unzipping lists
  , P.zip, P.zip3, P.zipWith, P.zipWith3, P.unzip, P.unzip3
    -- *** Functions on strings
  , P.lines, P.words, P.unlines, P.unwords
    -- ** Converting from and to @String@
    -- *** Converting to @String@
  , P.ShowS, P.Show(..)
  , P.shows, P.showChar, P.showString, P.showParen
    -- *** Converting from @String@
  , P.ReadS, P.Read(..)
  , P.reads, P.readParen, P.read, P.lex
    -- ** Basic input and output
  , P.IO
    -- *** Simple I/O operations
    -- **** Output functions
  , P.putChar, P.putStr, P.putStrLn, P.print
    -- **** Input functions
  , P.getChar, P.getLine, P.getContents, P.interact
    -- **** Files
  , P.FilePath
  , P.readFile, P.writeFile, P.appendFile, P.readIO, P.readLn
    -- *** Exception handing in the I/O monad
  , P.IOError
  , P.ioError, P.userError

    -- * Polymonads
  , module Control.Supermonad
  --, PM.Polymonad(..), PM.return, PM.fail
  --, PM.Identity(..), PM.runIdentity
    -- ** Replacement functions
  , F.mapM_, F.sequence_, (F.=<<)
    -- ** Traversable replacement functions
  , F.mapM, F.sequence
    -- * Fix rebindable syntax
  , ifThenElse
  ) where

import qualified Prelude as P

import Control.Supermonad
import qualified Control.Supermonad.Functions as F

-- | Standard implementation of if-then-else. Necessary because we are
--   going to use @RebindableSyntax@ together with this prelude.
ifThenElse :: P.Bool -> a -> a -> a
ifThenElse P.True  t _ = t
ifThenElse P.False _ f = f
