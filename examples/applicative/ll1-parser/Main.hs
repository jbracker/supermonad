
{-
  Implementation of an applicative LL(1) parser based on 
  the design presented in [1]. The important part is that this
  parser is actually just an applicative functor, not a monad.
  
  [1] S. Doaitse Swierstra and Luc Duponcheel, "Deterministic, Error-Correcting Combinator Parsers", 1996
      http://www.staff.science.uu.nl/~swier101/Papers/1996/DetErrCorrComPars.pdf
-}

import Data.List ( nub )
import Data.Char ( isAlpha, isDigit, isSpace )

import Control.Applicative ( Applicative(..), Alternative(..) )
import Control.Monad ( forM_, guard )

-- Parser with additional static information.
--   pEmpty   : Does the parser require to consume input?
--   pAccepted: Is the given token accepted by the parser? Predicate to judge lookahead.
--   pParse   : Actual parsing function.
data Parser s a = P { pEmpty :: Bool, pAccepted :: s -> Bool, pParse :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f (P empty accept p) = P empty accept $ \ss -> fmap (\(a, ss') -> (f a, ss')) $ p ss

instance Applicative (Parser s) where
  pure a = P True (const True) $ \ss -> Just (a, ss)
  ~p1@(P empty1 accept1 _) <*> ~p2@(P empty2 accept2 _) = 
    P (empty1 && empty2) (\s -> accept1 s || if empty1 then accept2 s else False) $ \ss1 -> do
      (f, ss2) <- execDP p1 ss1
      (a, ss3) <- execDP p2 ss2
      return (f a, ss3)
    where
      execDP :: Parser s a -> [s] -> Maybe (a, [s])
      execDP ~(P empty accepts p) ss = case ss of
        [] | empty     -> p ss
           | otherwise -> Nothing
        (s:_) | accepts s -> p ss
              | otherwise -> Nothing

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
parse ~(P empty accepts p) [] 
  | empty = p []
  | otherwise = Nothing
parse (P empty accepts p) ss@(s:_)
  | accepts s || empty = p ss
  | otherwise  = Nothing

anyOf :: (Eq s) => [s] -> Parser s s
anyOf xs = predicate (`elem` xs)

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
