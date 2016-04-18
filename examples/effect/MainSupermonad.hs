
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE UndecidableInstances #-}

-- Ignore our orphan instance in this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use the polymonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude

import Control.Effect ( Plus, Inv )
import qualified Control.Effect as E
import Control.Effect.State

instance ( Inv State s '[]
         , s ~ Plus State s '[]
         ) => Functor (State (s :: [*])) where
  fmap f ma = ma E.>>= (E.return . f)

instance ( h ~ Plus State f g
         , Inv State f g
         , f ~ Plus State f '[]
         , g ~ Plus State g '[]
         , h ~ Plus State h '[]
         , Inv State f '[]
         , Inv State g '[]
         , Inv State h '[]
         ) => Bind (State (f :: [*])) (State (g :: [*])) (State (h :: [*])) where
  (>>=) = (E.>>=)

instance (h ~ '[]) => Return (State (h :: [*])) where
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
incC = do { x <- get varC; put varC (x + 1); return (x + 1) }

writeS :: [a] -> State '["out" :-> [a] :! 'RW] ()
writeS y = do { x <- get varS; put varS (x ++ y) }

write :: [a] -> State '["count" :-> Int :! 'RW, "out" :-> [a] :! 'RW] ()
write x = do { writeS x; _ <- incC; return () }
