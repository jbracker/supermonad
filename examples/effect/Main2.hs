
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Prelude
import Prelude as P

import Control.Effect as E
import Control.Effect.Reader

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t e = t
ifThenElse False t e = e

main :: IO ()
main = do
  let l = runReader (flatFilter tree) (Ext vThres 3 Empty)
  print l
  print (sum l)
  where (>>) = (P.>>)
        return :: a -> IO a
        return = P.return

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
  where
    (>>=) :: (E.Inv Reader f g) => Reader f a -> (a -> Reader g b) -> Reader (E.Plus Reader f g) b
    (>>=) = (E.>>=)
    return = E.return
    fail = E.fail
flatFilter ( Branch l r ) = do
  ls <- flatFilter l
  rs <- flatFilter r
  return (ls ++ rs)
  where
    (>>=) :: (E.Inv Reader f g) => Reader f a -> (a -> Reader g b) -> Reader (E.Plus Reader f g) b
    (>>=) = (E.>>=)
    return = E.return
    fail = E.fail
