
import Data.List ( nub )
import Data.Set ( Set )
import qualified Data.Set as S

import Control.Applicative ( Applicative(..), Alternative(..) )


main :: IO ()
main = return ()


data StaticParser s a = SP Bool (Set s)

data DynamicParser s a = DP ([s] -> Maybe (a, [s]))

data Parser s a = P (StaticParser s a) (DynamicParser s a)

instance Functor (StaticParser s) where
  fmap _f (SP empty starters) = SP empty starters
instance (Ord s) => Applicative (StaticParser s) where
  pure _a = SP True S.empty
  (SP empty1 starters1) <*> (SP empty2 starters2) = SP (empty1 && empty2) $ starters1 `S.union` if empty1 then starters2 else S.empty
instance (Ord s) => Alternative (StaticParser s) where
  empty = SP True S.empty
  (SP empty1 starters1) <|> (SP empty2 starters2) = SP (empty1 || empty2) $ starters1 `S.union` starters2

symbolSP :: s -> StaticParser s a
symbolSP s = SP False $ S.singleton s

instance Functor (DynamicParser s) where
  fmap f (DP p) = DP $ \ss -> fmap (\(a, ss') -> (f a, ss')) $ p ss
instance Applicative (DynamicParser s) where
  pure a = DP $ \ss -> Just (a, ss)
  (DP p1) <*> (DP p2) = DP $ \ss0 -> do
    (f, ss1) <- p1 ss0
    (a, ss2) <- p2 ss1
    return (f a, ss2)

emptyDP :: DynamicParser s a
emptyDP = DP $ \_ss -> Nothing

alternativeDP :: (Eq s) => StaticParser s a -> StaticParser s a 
              -> DynamicParser s a -> DynamicParser s a -> DynamicParser s a
alternativeDP (SP empty1 starters1) (SP empty2 starters2) (DP p1) (DP p2) =
  DP $ \ss -> case ss of
    [] | empty1 -> p1 []
       | empty2 -> p2 []
    x:_ | x `elem` starters1 -> p1 ss 
        | x `elem` starters2 -> p2 ss
        | empty1 -> p1 ss
        | empty2 -> p2 ss
    _ -> Nothing

symbolDP :: s -> DynamicParser s s
symbolDP s = DP $ \(_:xs) -> Just (s, xs)

instance Functor (Parser s) where
  fmap f (P sp dp) = P (fmap f sp) (fmap f dp)
  
instance (Ord s) => Applicative (Parser s) where
  pure a = P (pure a) (pure a)
  (P sp1 dp1) <*> (P sp2 dp2) = P (sp1 <*> sp2) (dp1 <*> dp2)

instance (Ord s) => Alternative (Parser s) where
  empty = P empty emptyDP
  (P sp1 dp1) <|> (P sp2 dp2) = P (sp1 <|> sp2) (alternativeDP sp1 sp2 dp1 dp2)

symbol :: s -> Parser s s
symbol s = P (symbolSP s) (symbolDP s)

string :: (Ord s) => [s] -> a -> Parser s a
string [] a = pure a
string (s:ss) a = symbol s *> string ss a

-- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
-- We cannot implement bind, because we require the 'StaticParser'
-- information from the first and the second argument. But we 
-- can only get the static information from the second parser 
-- by supplying an 'a' to the function, which in turn, can 
-- only be produced if we actually execute the first argument.