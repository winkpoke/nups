module Techtron.JSON where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char (chr, isDigit, isHexDigit, isSpace)
import Numeric (readHex)

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

type Loc = (Int, Int)
type Input = (String, Loc)
type Err = (String, Loc)
type Ok a = (String, Loc, a)
newtype Parser a = Parser {runParser :: Input -> Either Err (Ok a)}

instance Functor Parser where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (Parser p) = Parser t
    where
      t x = do
        (s, loc, a) <- p x
        return (s, loc, f a)

instance Applicative Parser where
  -- pure :: a -> f a
  pure a = Parser (\(s, loc) -> Right (s, loc, a))

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Parser f) <*> (Parser p) = Parser t
    where
      t input = do
        (s', loc', f') <- f input
        (s'', loc'', x'') <- p (s', loc')
        Right (s'', loc'', f' x'')
        
instance Alternative Parser where
  -- empty :: f a
  empty = Parser (const $ Left ("Error.", (1, 1)))

  -- (<|>) :: f a -> f a -> f a
  (Parser p0) <|> (Parser p1) = Parser (
      \input -> 
          let a = p0 input in
          let b = p1 input in
          case a of
              Left _  -> case b of 
                            Left _  -> a
                            Right _ -> b
              Right _ -> a)

instance Monad Parser where
  -- return :: Monad m => a -> m a
  return a = pure a

  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (Parser p) >>= f = Parser t
    where t x = do
            (s', loc', a) <- p x
            let (Parser p') = f a
            p' (s', loc')

charIf :: (Char -> Bool) -> Parser Char
charIf pred = Parser f
    where
        f (x : xs, (line, pos))
            | pred x    = case x of 
                            '\n' -> Right (xs, (line + 1, 1), x)
                            _    -> Right (xs, (line, pos + 1), x)
            | otherwise = Left ("Error when parsing: " ++ [x], (line, pos))
        f ([], (line, pos)) = Left ("Error: exhauseted.", (line, pos))
        
char :: Char -> Parser Char
char x = charIf (== x)

string :: String -> Parser String
string = traverse char


normalChar :: Parser Char
-- normalChar = charIf (liftA2 (&&) (liftA2 (&&) (not . isControl) (/= '"')) (/= '\\'))
normalChar = charIf (liftA2 (&&) (/= '"') (/= '\\'))

escapeChar :: Parser Char
escapeChar =
  '\n' <$ string "\\n" <|> '\\' <$ string "\\\\" <|> '"' <$ string "\\\""
    <|> '\t' <$ string "\\t"
    <|> '/' <$ string "\\/"
    <|> '\b' <$ string "\\b"
    <|> '\b' <$ string "\\b"
    <|> '\r' <$ string "\\r"
    <|> string "\\u" *> (chr . fst . head . readHex <$> replicateM 4 (charIf isHexDigit))
    <|> char '\\' *> normalChar

stringP :: Parser String
stringP = char '"' *> spanP (/= '"') <* char '"'

jStringP :: Parser JValue
jStringP = JString <$> (char '"' *> many (normalChar <|> escapeChar) <* char '"')

ws :: Parser String
ws = spanP isSpace

jNullP :: Parser JValue
jNullP = JNull <$ string "null"

spanP :: (Char -> Bool) -> Parser String
spanP pred = many $ charIf pred

notNull :: Parser [a] -> Parser [a]
notNull (Parser a) =
  Parser $ \x -> do
    (s, loc, d) <- a x
    if null d 
        then Left ("Error: list is null", loc) 
        else return (s, loc, d)

jBoolP :: Parser JValue
jBoolP = JBool True <$ string "true" <|> JBool False <$ string "false"

(<:>) :: Parser a -> Parser [a] -> Parser [a]
a <:> b = (:) <$> a <*> b

jNumberP :: Parser JValue
jNumberP = JNumber <$> value
  where
    e = char 'e' <|> char 'E'
    minus = -1 <$ char '-' <|> pure 1
    plusMinus = -1 <$ char '-' <|> 1 <$ char '+' <|> pure 1
    digits = spanP isDigit
    oneToNine = foldl (<|>) empty (map char $ concatMap show [1..9])
      -- char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' 
      --          <|> char '6' <|> char '7' <|> char '8' <|> char '9'
    value =
      toNumber <$> minus
        <*> (read <$> (oneToNine <:> digits) <|> read <$> string "0")
        <*> fracToDouble  ((char '.' <:> notNull digits) <|> string "." <|> pure "0")
        <*> (e *> ((*) <$> plusMinus <*> (read <$> notNull digits)) <|> pure 0)
    fracToDouble :: Parser String -> Parser Double
    fracToDouble ps = do
        s <- ps
        if head s == '.' && not (null $ tail s)
          then return $ read $ '0' : s
          else if head s == '.' && null (tail s) then empty else return 0

toNumber :: Integer -> Integer -> Double -> Integer -> Double
toNumber sign dec frac exp = fromIntegral sign * (fromIntegral dec + frac) * 10 ^^ exp

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy item sep = (:) <$> item <*> many (sep *> item) <|> pure []

jArrayP :: Parser JValue
jArrayP = JArray <$> (char '[' *> ws *> (jValueP `separatedBy` sep) <* ws <* char ']')
  where
    sep = ws *> char ',' <* ws

jObjectP :: Parser JValue
jObjectP = JObject <$> (char '{' *> ws *> (pair `separatedBy` sep) <* ws <* char '}')
  where
    key = ws *> stringP <* ws
    sep = ws *> char ',' <* ws
    pair = (,) <$> key <* char ':' <*> jValueP

jValueP :: Parser JValue
jValueP = ws *> (jNullP <|> jStringP <|> jArrayP <|> jBoolP <|> jNumberP <|> jObjectP) <* ws

-- newtype Parser a = Parser {runParser :: Input -> Either Err (Ok a)}
parseJsonStr :: String -> Either Err (Ok JValue)
parseJsonStr s = runParser jValueP (s, (1, 1))
