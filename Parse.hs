module Parse where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as Map

type ParserContext = Map.Map String String
newtype Parser a = P (ParserContext -> String -> [(a, String, Map.Map String String)])

emptyParserContext :: ParserContext
emptyParserContext = Map.fromList []

parse :: Parser a -> ParserContext -> String -> [(a, String, ParserContext)]
parse (P p) ctx inp = p ctx inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\ctx inp -> case parse p ctx inp of
                  [] -> []
                  [(x, xs, c)] -> [(f x, xs, c)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\ctx inp -> [(x, inp, ctx)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fp <*> xp = P (\ctx inp -> case parse fp ctx inp of
                  [] -> []
                  [(f, xs, c)] -> parse (fmap f xp) c xs)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = P (\ctx inp -> case parse px ctx inp of
                  [] -> []
                  [(x, out, c)] -> parse (f x) c out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\ctx inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\ctx inp -> case parse p ctx inp of
                [] -> parse q ctx inp
                [(v, out, c)] -> [(v, out, c)])

varInContext :: String -> String -> ParserContext -> Bool
varInContext n t ctx = case Map.lookup n ctx of
                       (Just tt) -> t == tt
                       _ -> False

addValToParserContext :: String -> String -> ParserContext -> ParserContext
addValToParserContext n t ctx = Map.insert n t ctx

updateParserContext :: String -> String -> Parser a -> Parser a
updateParserContext n t p = P (\ctx inp -> parse p (addValToParserContext n t ctx) inp)

item :: Parser Char
item = P (\ctx inp -> case inp of
            [] -> []
            (x:xs) -> [(x, xs, ctx)])

sat :: (Char -> Bool) -> Parser Char
sat cond = do p <- item
              if (cond p) then return p else empty

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

variable :: Parser String
variable = do c <- letter
              do cs <- some alphanum
                 return (c:cs)
                <|> return (c:[])

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do c <- char x
                   cs <- string xs
                   return (c:cs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)