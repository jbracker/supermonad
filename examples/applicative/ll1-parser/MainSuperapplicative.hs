
{-
  Implementation of an applicative LL(1) parser based on 
  the design presented in [1]. The important part is that this
  parser is actually just an applicative functor, not a monad.
  This version uses superapplicatives instead of standard applicatives.
  
  [1] S. Doaitse Swierstra and Luc Duponcheel, "Deterministic, Error-Correcting Combinator Parsers", 1996
      http://www.staff.science.uu.nl/~swier101/Papers/1996/DetErrCorrComPars.pdf
-}

-- Use the supermonad plugin.
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin Control.Super.Monad.Plugin #-}

-- Required to define instances:
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Super.Monad.Prelude

import Data.Char ( isAlpha, isDigit, isSpace )

import Control.Applicative ( Alternative(..) )
import Control.Super.Monad.Functions ( forM_ )

-- TODO: Can be removed once a super-version of Alternative exists.
import qualified Prelude as P

-- Parser with additional static information:
--   * Does the parser require to consume input?
--   * Is the given token accepted by the parser? Predicate to judge lookahead.
--   * Actual parsing function.
data Parser s a = P Bool (s -> Bool) ([s] -> Maybe (a, [s]))

instance Functor (Parser s) where
  fmap f (P e accept p) = P e accept $ \ss -> fmap (\(a, ss') -> (f a, ss')) $ p ss

instance Return (Parser s) where
  return a = P True (const True) $ \ss -> Just (a, ss)

instance Applicative (Parser s) (Parser s) (Parser s) where
  ~p1@(P empty1 accept1 _) <*> ~p2@(P empty2 accept2 _) = 
    P (empty1 && empty2) (\s -> accept1 s || if empty1 then accept2 s else False) $ \ss1 -> do
      (f, ss2) <- execDP p1 ss1
      (a, ss3) <- execDP p2 ss2
      return (f a, ss3)
    where
      execDP :: Parser s a -> [s] -> Maybe (a, [s])
      execDP ~(P e accepts p) ss = case ss of
        [] | e         -> p ss
           | otherwise -> Nothing
        (s:_) | accepts s -> p ss
              | otherwise -> Nothing

-- TODO: We still need a super-version of Alternative, then we can replace this instance and remove the prelude implementation of applicative.
instance P.Applicative (Parser s) where
  pure = pure
  (<*>) = (<*>)
instance Alternative (Parser s) where
  empty = P False (const False) (const Nothing)
  ~(P empty1 accepts1 p1) <|> ~(P empty2 accepts2 p2) = 
    P (empty1 || empty2) (\s -> accepts1 s || accepts2 s) $ \ss -> case ss of
      [] | empty1 -> p1 []
         | empty2 -> p2 []
      x:_ | accepts1 x || empty1 -> p1 ss 
          | accepts2 x || empty2 -> p2 ss
      _ -> Nothing

-- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
-- We cannot implement bind, because we require the static
-- information from the first and the second argument. But we 
-- can only get the static information from the second parser 
-- by supplying an 'a' to the function, which in turn, can 
-- only be produced if we actually execute the first argument.
-- See the implementation of '<*>' for an example of their interdependence.

symbol :: (Eq s) => s -> Parser s s
symbol s = P False (== s) $ \(_:ss) -> Just (s, ss)

predicate :: (s -> Bool) -> Parser s s
predicate p = P False p $ \(s:ss) -> Just (s, ss)

parse :: Parser s a -> [s] -> Maybe (a, [s])
parse ~(P e _accepts p) [] 
  | e         = p []
  | otherwise = Nothing
parse (P e accepts p) ss@(s:_)
  | accepts s || e = p ss
  | otherwise      = Nothing

equal :: Parser Char Token
equal = symbol '=' *> pure TEqual

semicolon :: Parser Char Token
semicolon = symbol ';' *> pure TSemicolon

ident :: Parser Char Token
ident = TIdent <$> some (predicate $ \s -> isAlpha s || '_' == s)

whitespaces :: Parser Char ()
whitespaces =  many (predicate isSpace) *> pure ()

number :: Parser Char Token
number = (TNumber . read) <$> some (predicate isDigit)

data Token = TNumber Int
           | TIdent String
           | TEqual
           | TSemicolon
           deriving (Show, Eq, Ord)

isIdent, isNumber :: Token -> Bool
isIdent  t = case t of { TIdent  _ -> True ; _ -> False }
isNumber t = case t of { TNumber _ -> True ; _ -> False }

tokenParser :: Parser Char [Token]
tokenParser = many $ (ident <|> number <|> equal <|> semicolon) <* whitespaces

parseTokens :: String -> Maybe ([Token], String)
parseTokens = parse (whitespaces *> tokenParser)

identTok :: Parser Token String
identTok = (\(TIdent i) -> i) <$> predicate isIdent

numberTok :: Parser Token Int
numberTok = (\(TNumber i) -> i) <$> predicate isNumber

assocParser :: Parser Token [(String, Int)]
assocParser = many $ (,) <$> identTok <* symbol TEqual <*> numberTok <* symbol TSemicolon

parseAssocs :: String -> Maybe [(String, Int)]
parseAssocs str = do 
  (toks, restStr) <- parseTokens str
  guard $ null restStr
  (res, restToks) <- parse assocParser toks
  guard $ null restToks
  return res
  where
    -- TODO: Once we have super-version of Alternative we also give a generalized version of guard.
    guard :: Bool -> Maybe ()
    guard True = pure ()
    guard False = empty


main :: IO ()
main = do
  let tests = 
        [ ("abc = 45 ;ddd=3;   ", Just [("abc", 45), ("ddd", 3)])
        , ("", Just [])
        , ("     ", Just [])
        , ("x = 1; ; = ;", Nothing)
        , ("@" , Nothing)
        , ("___=4;_AB=22222222;" , Just [("___", 4),("_AB", 22222222)])
        , ("___=4;_AB=22222222;@" , Nothing)
        ]
  
  let test :: (String, Maybe [(String, Int)]) -> IO ()
      test (str, res) = do
        let parseRes = parseAssocs str
        if parseAssocs str == res then do
          putStr "."
        else do
          putStrLn "" >> putStrLn "Test failed:"
          putStrLn $ "  Test case:" 
          putStrLn $ "    \"" ++ str ++ "\"" 
          putStrLn $ "  Exprected result: " 
          putStrLn $ "    " ++ show res
          putStrLn $ "  Actual result: "
          putStrLn $ "    " ++ show parseRes
  
  forM_ tests test
  putStrLn ""
