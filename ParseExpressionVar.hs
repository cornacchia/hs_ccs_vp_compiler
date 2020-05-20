module ParseExpressionVar where
import Control.Applicative
import Parse

data Expr
  = EInt IntegerExpression
  | EBool BooleanExpression
  deriving (Show, Eq)

data IntegerExpression
  = EMult IntegerExpression IntegerExpression
  | EDiv IntegerExpression IntegerExpression
  | ESum IntegerExpression IntegerExpression
  | EDiff IntegerExpression IntegerExpression
  | ENegate IntegerExpression
  | EIntNat Int
  | EIntVar String
  deriving (Show, Eq)

data BooleanExpression
  = EAnd BooleanExpression BooleanExpression
  | EOr BooleanExpression BooleanExpression
  | ENot BooleanExpression
  | EBEq BooleanExpression BooleanExpression
  | EBNeq BooleanExpression BooleanExpression
  | EIEq IntegerExpression IntegerExpression
  | EINeq IntegerExpression IntegerExpression
  | EIGt IntegerExpression IntegerExpression
  | EIGet IntegerExpression IntegerExpression
  | EILt IntegerExpression IntegerExpression
  | EILet IntegerExpression IntegerExpression
  | EBoolValue Bool
  | EBoolVar String
  deriving (Show, Eq)

parseExprParens :: Parser [Expr]
parseExprParens = do symbol "("
                     do exprs <- parseExprs
                        symbol ")"
                        return exprs
                       <|> do symbol ")"
                              return []

parseExprs :: Parser [Expr]
parseExprs = do e <- parseExpression
                do symbol ","
                   es <- parseExprs
                   return (e:es)
                  <|> return [e]

parseExpression :: Parser Expr
parseExpression = parseBoolExpr <|> parseIntExpr

parseIntExpr :: Parser Expr
parseIntExpr = do e1 <- parseInt1
                  return (EInt e1)

parseSumSymbols :: Parser String
parseSumSymbols = symbol "+" <|> symbol "-"

parseInt1 :: Parser IntegerExpression
parseInt1 = do e1 <- parseInt2
               do s <- parseSumSymbols
                  e2 <- parseInt1
                  return (case s of
                          "+" -> (ESum e1 e2)
                          "-" -> (EDiff e1 e2))
                 <|> return e1

parseMultiplicationSymbol :: Parser String
parseMultiplicationSymbol = symbol "*" <|> symbol "/"

parseInt2 :: Parser IntegerExpression
parseInt2 = do e1 <- parseInt3
               do s <- parseMultiplicationSymbol
                  e2 <- parseInt2
                  return (case s of
                          "*" -> (EMult e1 e2)
                          "/" ->  (EDiv e1 e2))
                 <|> return e1

parseInt3 :: Parser IntegerExpression
parseInt3 = do symbol "-"
               e1 <- parseInt4
               return (ENegate e1)
              <|> parseInt4

parseInt4 :: Parser IntegerExpression
parseInt4 = parseIntExprParens <|> parseIntExprNat <|> parseIntExprVar

parseIntExprParens :: Parser IntegerExpression
parseIntExprParens = do symbol "("
                        e <- parseInt1
                        symbol ")"
                        return e

parseIntExprNat :: Parser IntegerExpression
parseIntExprNat = do n <- nat
                     return (EIntNat n)

parseIntExprVar :: Parser IntegerExpression
parseIntExprVar = do v <- token variable
                     return (EIntVar v)

parseBoolExpr :: Parser Expr
parseBoolExpr = do b <- parseBool0
                   return (EBool b)

parseBool0 :: Parser BooleanExpression
parseBool0 = parseIntComparison <|> parseBoolComparison <|> parseBool1

parseBool1 :: Parser BooleanExpression
parseBool1 = do e1 <- parseBool2
                do symbol "||"
                   e2 <- parseBool1
                   return (EOr e1 e2)
                  <|> return e1

parseBool2 :: Parser BooleanExpression
parseBool2 = do e1 <- parseBool3
                do symbol "&&"
                   e2 <- parseBool2
                   return (EAnd e1 e2)
                  <|> return e1

parseBool3 :: Parser BooleanExpression
parseBool3 = do symbol "!"
                e <- parseBool4
                return (ENot e)
               <|> parseBool4

parseBool4 :: Parser BooleanExpression
parseBool4 = parseTrue <|> parseFalse <|> parseBoolExprParens

parseTrue :: Parser BooleanExpression
parseTrue = do symbol "True"
               return (EBoolValue True)

parseFalse :: Parser BooleanExpression
parseFalse = do symbol "False"
                return (EBoolValue False)

parseBoolExprParens :: Parser BooleanExpression
parseBoolExprParens = do symbol "("
                         e <- parseBool0
                         symbol ")"
                         return e

parseBoolExprVar :: Parser BooleanExpression
parseBoolExprVar = do v <- token (some alphanum)
                      return (EBoolVar v)

parseComparisonSymbol :: Parser String
parseComparisonSymbol = symbol "==" <|> symbol "!=" <|> symbol "<=" <|> symbol "<" <|> symbol ">=" <|> symbol ">"

parseIntComparisonSymbol :: Parser String
parseIntComparisonSymbol = parseComparisonSymbol <|> symbol "<=" <|> symbol "<" <|> symbol ">=" <|> symbol ">"

parseIntComparison :: Parser BooleanExpression
parseIntComparison = do e1 <- parseInt1
                        s <- parseIntComparisonSymbol
                        e2 <- parseInt1
                        return (case s of
                                "==" -> (EIEq e1 e2)
                                "!=" -> (EINeq e1 e2)
                                "<=" -> (EILet e1 e2)
                                "<" -> (EILt e1 e2)
                                ">=" -> (EIGet e1 e2)
                                ">" -> (EIGt e1 e2))

parseBoolComparison :: Parser BooleanExpression
parseBoolComparison = do e1 <- parseBool1
                         s <- parseComparisonSymbol
                         e2 <- parseBool1
                         return (case s of
                                 "==" -> (EBEq e1 e2)
                                 "!=" -> (EBNeq e1 e2))