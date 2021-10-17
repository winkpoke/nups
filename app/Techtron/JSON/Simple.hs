module Techtron.JSON.Simple 
( parseJsonStr
, JValue (..)
, jValueP
, Parser (..)
, charIf
, char
, string
) where

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

newtype Parser a = Parser {parse :: String -> Maybe (String, a)}


instance Functor Parser where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (Parser p) = Parser t
    where
      t x = do
        (x', a) <- p x
        return (x', f a)

instance Applicative Parser where
  -- pure :: a -> f a
  pure a = Parser (\x -> Just (x, a))

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Parser f) <*> (Parser p) = Parser t
    where
      t x = do
        (x', f') <- f x
        (x'', x) <- p x'
        Just (x'', f' x)

instance Alternative Parser where
  -- empty :: f a
  empty = Parser (const Nothing)

  -- (<|>) :: f a -> f a -> f a
  (Parser p0) <|> (Parser p1) = Parser (\input -> p0 input <|> p1 input)

instance Monad Parser where
  -- return :: Monad m => a -> m a
  return a = pure a

  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (Parser p) >>= f = Parser t
    where t x = do
            (x', a) <- p x
            let (Parser p') = f a
            p' x'

char :: Char -> Parser Char
char x = charIf (== x)

charIf :: (Char -> Bool) -> Parser Char
charIf pred = Parser f
  where
    f (x : xs)
      | pred x = Just (xs, x)
      | otherwise = Nothing
    f [] = Nothing

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

string :: String -> Parser String
string = traverse char

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
    (c, d) <- a x
    if null d then Nothing else return (c, d)

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

parseJsonStr :: String -> Maybe (String, JValue)
parseJsonStr = parse jValueP 
