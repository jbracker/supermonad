
{-# LANGUAGE CPP #-}

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0)
-- Required for the 'Bind' instance since GHC 8.0.1
{-# LANGUAGE UndecidableInstances #-}
#endif

-- Ignore our orphan instance in this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use the supermonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude

import qualified Control.Effect as E
import Control.Effect ( Plus, Inv )
import Control.Effect.Reader

import GHC.TypeLits ( Symbol )

instance Functor (Reader (s :: [Mapping Symbol *])) where
  fmap f ma = IxR $ \s -> f $ runReader ma s

instance ( h ~ Plus Reader f g) => Bind (Reader (f :: [Mapping Symbol *])) (Reader (g :: [Mapping Symbol *])) (Reader (h :: [Mapping Symbol *])) where
  type BindCts (Reader (f :: [Mapping Symbol *])) (Reader (g :: [Mapping Symbol *])) (Reader (h :: [Mapping Symbol *])) = Inv Reader f g
  (>>=) = (E.>>=)

instance Return (Reader '[]) where
  return = E.return

instance Fail (Reader (h :: [Mapping Symbol *])) where
  fail = E.fail

main :: IO ()
main = do
  let l = runReader (flatFilter tree) (Ext vThres 3 Empty)
  print l
  print (sum l)

vThres :: Var "thres"
vThres = Var

data Tree = Leaf Int
          | Branch Tree Tree
          deriving Show

tree :: Tree
tree = Branch (Branch (Leaf 1) (Leaf 4)) (Leaf 5)

flatFilter :: Tree -> Reader '["thres" ':-> Int] [Int]
flatFilter ( Leaf i ) = do
  thres <- ask vThres
  return (if i < thres then [] else [i])
flatFilter ( Branch l r ) = do
  ls <- flatFilter l
  rs <- flatFilter r
  return (ls ++ rs)
