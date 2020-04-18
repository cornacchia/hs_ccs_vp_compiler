module ParseExpression where
import Control.Applicative
import Parse

data Expr
  = EInt Int
  | EBool Bool
  deriving Show

parseExpression :: Parser Expr
parseExpression = parseIntExpr <|> parseBoolExpr

parseIntExpr :: Parser Expr
parseIntExpr = do n <- parseInt1
                  return (EInt n)

parseSumSymbols :: Parser String
parseSumSymbols = symbol "+" <|> symbol "-"

parseInt1 :: Parser Int
parseInt1 = do n1 <- parseInt2
               do s <- parseSumSymbols
                  n2 <- parseInt1
                  return (case s of
                          "+" -> n1 + n2
                          "-" -> n1 - n2)
                 <|> return n1

parseMultiplicationSymbol :: Parser String
parseMultiplicationSymbol = symbol "*" <|> symbol "/"

parseInt2 :: Parser Int
parseInt2 = do n1 <- parseInt3
               do s <- parseMultiplicationSymbol
                  n2 <- parseInt2
                  return (case s of
                          "*" -> n1 * n2
                          "/" ->  (div n1 n2))
                 <|> return n1

parseInt3 :: Parser Int
parseInt3 = do symbol "-"
               n1 <- parseInt4
               return (negate n1)
              <|> parseInt4

parseInt4 :: Parser Int
parseInt4 = do symbol "("
               n <- parseInt1
               symbol ")"
               return n
              <|> nat

parseBoolExpr :: Parser Expr
parseBoolExpr = do b <- parseBool1
                   return (EBool b)

parseBool1 :: Parser Bool
parseBool1 = do b1 <- parseBool2
                do symbol "||"
                   b2 <- parseBool1
                   return (b1 || b2)
                  <|> return b1

parseBool2 :: Parser Bool
parseBool2 = do b1 <- parseBool3
                do symbol "&&"
                   b2 <- parseBool2
                   return (b1 && b2)
                  <|> return b1

parseBool3 :: Parser Bool
parseBool3 = do symbol "!"
                b <- parseBool4
                return (not b)
               <|> parseBool4

parseBool4 :: Parser Bool
parseBool4 = parseTrue <|> parseFalse <|>
             do symbol "("
                do b <- parseBool1
                   symbol ")"
                   return b
                  <|> do b <- parseEquality parseInt1
                         symbol ")"
                         return b
                        <|> do b <- parseEquality parseBool1
                               symbol ")"
                               return b
                              <|> do b <- parseComparison parseInt1
                                     symbol ")"
                                     return b

parseTrue :: Parser Bool
parseTrue = do symbol "True"
               return True

parseFalse :: Parser Bool
parseFalse = do symbol "False"
                return False

parseEqualitySymbol :: Parser String
parseEqualitySymbol = symbol "==" <|> symbol "!="

parseEquality :: Eq a => Parser a -> Parser Bool
parseEquality parser = do e1 <- parser
                          s <- parseEqualitySymbol
                          e2 <- parser
                          return (case s of
                                  "==" -> e1 == e2
                                  "!=" -> e1 /= e2)

parseComparisonSymbol :: Parser String
parseComparisonSymbol = symbol "<" <|> symbol "<=" <|> symbol ">" <|> symbol ">="

parseComparison :: Ord a => Parser a -> Parser Bool
parseComparison parser = do e1 <- parser
                            s <- parseComparisonSymbol
                            e2 <- parser
                            return (case s of
                                    "<" -> e1 < e2
                                    "<=" -> e1 <= e2
                                    ">" -> e1 > e2
                                    ">=" -> e1 >= e2)