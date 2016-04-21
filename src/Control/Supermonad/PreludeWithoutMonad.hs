
-- | A verson of the standard prelude that does not export anything that 
--   involves standard monads.
module Control.Supermonad.PreludeWithoutMonad
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
  ) where

import qualified Prelude as P