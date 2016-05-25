
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

-- Use the polymonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude

import Control.Effect ( Plus, Inv )
import qualified Control.Effect as E
import Control.Effect.State

instance Functor (State (s :: [*])) where
  fmap f ma = State $ \s -> let (a, s') = runState ma s in (f a, s') 

instance ( h ~ Plus State f g ) => Bind (State (f :: [*])) (State (g :: [*])) (State (h :: [*])) where
  type BindCts (State (f :: [*])) (State (g :: [*])) (State (h :: [*])) = Inv State f g
  (>>=) = (E.>>=)

instance Return (State '[]) where
  return = E.return

instance Fail (State (h :: [*])) where
  fail = E.fail

main :: IO ()
main = do
  putStrLn $ show $ runState
    ( write "abc" )
    ( Ext (Var :-> 0 :! Eff) (Ext (Var :-> [] :! Eff) Empty) )

varC :: Var "count"
varC = Var 
varS :: Var "out"
varS = Var

incC :: State '["count" :-> Int :! 'RW] Int
incC = do 
  x <- get varC
  put varC (x + 1)
  return (x + 1)

writeS :: [a] -> State '["out" :-> [a] :! 'RW] ()
writeS y = do 
  x <- get varS
  put varS (x ++ y)

write :: [a] -> State '["count" :-> Int :! 'RW, "out" :-> [a] :! 'RW] ()
write x = do 
  writeS x
  _ <- incC
  return ()
