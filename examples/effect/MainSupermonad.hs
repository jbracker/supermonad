
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-}

-- Ignore our orphan instance in this file.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use the polymonad plugin.
{-# OPTIONS_GHC -fplugin Control.Supermonad.Plugin #-}

import Control.Supermonad.Prelude

import Control.Effect ( Effect, Plus, Unit, Inv )
import qualified Control.Effect as E
import Control.Effect.State

instance ( Inv State s '[]
         , s ~ Plus State s '[]
         ) => Functor (State (s :: [*])) where
  fmap f ma = ma E.>>= (E.return . f)
-- Inv State f g
instance ( Inv State f g
         , Inv State f '[]
         , f ~ Plus State f '[]
         , Inv State g '[]
         , g ~ Plus State g '[]
         , Inv State (Plus State f g) '[]
         , Plus State (Plus State f g) '[] ~ (Plus State f g)
         ) => Bind (State (f :: [*])) (State (g :: [*])) where
  type BindF (State f) (State g) = State (Plus State f g)
  (>>=) = (E.>>=)

instance (Effect m, h ~ Unit m) => Return (m (h :: k)) where
  return = E.return

instance Fail (m (h :: k)) where
  fail = E.fail

main :: IO ()
main = do
  putStrLn $ show $ runState
    ( write "abc" )
    ( Ext (Var :-> 0 :! Eff) (Ext (Var :-> [] :! Eff) Empty) )

varC = Var :: Var "count"
varS = Var :: Var "out"

incC :: State '["count" :-> Int :! RW] Int
incC = do { x <- get varC; put varC (x + 1); return (x + 1) }

writeS :: [a] -> State '["out" :-> [a] :! RW] ()
writeS y = do { x <- get varS; put varS (x ++ y) }

write :: [a] -> State '["count" :-> Int :! RW, "out" :-> [a] :! RW] ()
write x = do { writeS x; _ <- incC; return () }
