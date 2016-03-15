
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

import Control.Effect ( Effect, Plus, Unit, Inv )
import qualified Control.Effect as E
import Control.Effect.State

instance ( Effect m
         , Inv m s (Unit m)
         , s ~ Plus m s (Unit m)
         ) => Functor (m (s :: k)) where
  fmap f ma = ma E.>>= (E.return . f)

instance ( Effect m
         , h ~ Plus m f g
         , Inv m f g
         , f ~ Plus m f (Unit m)
         , g ~ Plus m g (Unit m)
         , h ~ Plus m h (Unit m)
         , Inv m f (Unit m)
         , Inv m g (Unit m)
         , Inv m h (Unit m)
         ) => Bind (m (f :: k)) (m (g :: k)) (m (h :: k)) where
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
