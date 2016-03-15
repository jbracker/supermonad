
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main, write ) where

import Prelude
import qualified Prelude as P
import qualified Control.Effect as E
import Control.Effect.State

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t e = t
ifThenElse False t e = e

main :: IO ()
main = do
  putStrLn $ show $ runState
    ( write "abc" )
    ( Ext (Var :-> 0 :! Eff) (Ext (Var :-> [] :! Eff) Empty) )

varC = Var :: Var "count"
varS = Var :: Var "out"

incC :: State '["count" :-> Int :! RW] Int
incC = do { x <- get varC; put varC (x + 1); return (x + 1) }
  where (>>=) :: (E.Inv State f g) => State f a -> (a -> State g b) -> State (E.Plus State f g) b
        (>>=) = (E.>>=)
        (>>) :: (E.Inv State f g) => State f a -> State g b -> State (E.Plus State f g) b
        (>>) = (E.>>)
        return :: a -> State '[] a
        return = E.return
        fail = E.fail

writeS :: [a] -> State '["out" :-> [a] :! RW] ()
writeS y = do { x <- get varS; put varS (x ++ y) }
  where (>>=) :: (E.Inv State f g) => State f a -> (a -> State g b) -> State (E.Plus State f g) b
        (>>=) = (E.>>=)
        (>>) :: (E.Inv State f g) => State f a -> State g b -> State (E.Plus State f g) b
        (>>) = (E.>>)
        return :: a -> State '[] a
        return = E.return
        fail = E.fail

write :: [a] -> State '["count" :-> Int :! RW, "out" :-> [a] :! RW] ()
write x = do { writeS x; _ <- incC; return () }
  where (>>=) :: (E.Inv State f g) => State f a -> (a -> State g b) -> State (E.Plus State f g) b
        (>>=) = (E.>>=)
        (>>) :: (E.Inv State f g) => State f a -> State g b -> State (E.Plus State f g) b
        (>>) = (E.>>)
        return :: a -> State '[] a
        return = E.return
        fail = E.fail
