module ParseProg where
import Control.Applicative
import Parse

data Expr
  = EInt Int
  | EBool Bool
  deriving Show

instance Ord Expr where
    EInt n1 < EInt n2 = n1 < n2
    EBool False < EBool True = True
    b <= c = (b < c) || (b == c)
    b > c = c < b
    b >= c = c <= b

instance Eq Expr where
    EInt n1 == EInt n2 = n1 == n2
    EBool b1 == EBool b2 = b1 == b2
    _ == _ = False

exprSum :: Expr -> Expr -> Expr
exprSum (EInt n1) (EInt n2) = EInt (n1 + n2)
exprSum _ _ = error "Incorrect Integer SUM operands"

exprDiff :: Expr -> Expr -> Expr
exprDiff (EInt n1) (EInt n2) = EInt (n1 - n2)
exprDiff _ _ = error "Incorrect Integer DIFFERENCE operands"

exprMultiply :: Expr -> Expr -> Expr
exprMultiply (EInt n1) (EInt n2) = EInt (n1 * n2)
exprMultiply _ _ = error "Incorrect Integer MULTIPLICATION operands"

exprDivide :: Expr -> Expr -> Expr
exprDivide (EInt n1) (EInt n2) = EInt (div n1 n2)
exprDivide _ _ = error "Incorrect Integer DIVISION operands"

exprNegate :: Expr -> Expr
exprNegate (EInt n1) = EInt (negate n1)
exprNegate _ = error "Incorrect Integer NEGATE operand"

exprAnd :: Expr -> Expr -> Expr
exprAnd (EBool b1) (EBool b2) = EBool (b1 && b2)
exprAnd _ _ = error "Incorrect Boolean AND operands"

exprOr :: Expr -> Expr -> Expr
exprOr (EBool b1) (EBool b2) = EBool (b1 || b2)
exprOr _ _ = error "Incorrect Boolean OR operands"

exprNot :: Expr -> Expr
exprNot (EBool b1) = EBool (not b1)
exprNot _ = error "Incorrect Boolean NOT operands"

parseEqualitySymbol :: Parser String
parseEqualitySymbol = symbol "!=" <|> symbol "=="

parseComparisonSymbol :: Parser String
parseComparisonSymbol = symbol "<=" <|> symbol "<" <|> symbol ">=" <|> symbol ">"

parseAdditionSymbol :: Parser String
parseAdditionSymbol = symbol "+"  <|> symbol "-"

parseMultiplicationSymbol :: Parser String
parseMultiplicationSymbol = symbol "*" <|> symbol "/"

parseExprs :: Parser [Expr]
parseExprs = do e <- parseExpr
                do symbol ","
                   es <- parseExprs
                   return (e:es)
                  <|> return [e]

parseExpr :: Parser Expr
parseExpr = do c1 <- parseSubExpression
               do symbol <- parseEqualitySymbol
                  c2 <- parseSubExpression
                  return (case symbol of
                          "==" -> EBool (c1 == c2)
                          "!=" -> EBool (c1 /= c2))
                 <|> return c1

parseSubExpression :: Parser Expr
parseSubExpression = parseBooleanExpr <|> parseComparison

parseBooleanExpr :: Parser Expr
parseBooleanExpr = do t1 <- parseBooleanTerm
                      do symbol "||"
                         t2 <- parseBooleanTerm
                         return (exprOr t1 t2)
                        <|> return t1

parseBooleanTerm :: Parser Expr
parseBooleanTerm = do f1 <- parseBooleanFactor
                      do symbol "&&"
                         f2 <- parseBooleanFactor
                         return (exprAnd f1 f2)
                        <|> return f1

parseBooleanFactor :: Parser Expr
parseBooleanFactor = parseTrue <|> parseFalse <|> parseNot <|> parseParens

parseTrue :: Parser Expr
parseTrue = do symbol "True"
               return (EBool True)

parseFalse :: Parser Expr
parseFalse = do symbol "False"
                return (EBool False)

parseNot :: Parser Expr
parseNot = do symbol "!"
              f <- parseBooleanFactor
              return (exprNot f)

parseParens :: Parser Expr
parseParens = do symbol "("
                 e <- parseExpr
                 symbol ")"
                 return e

parseComparison :: Parser Expr
parseComparison = do a1 <- parseAddition
                     do symbol <- parseComparisonSymbol
                        a2 <- parseAddition
                        return (case symbol of
                                "<=" -> EBool (a1 <= a2)
                                "<" -> EBool (a1 < a2)
                                ">=" -> EBool (a1 >= a2)
                                ">" -> EBool (a1 > a2))
                       <|> return a1

parseAddition :: Parser Expr
parseAddition = do m1 <- parseMultiplication
                   do symbol <- parseAdditionSymbol
                      m2 <- parseMultiplication
                      return (case symbol of
                              "+" -> exprSum m1 m2
                              "-" -> exprDiff m1 m2)
                     <|> return m1

parseMultiplication :: Parser Expr
parseMultiplication = do u1 <- parseUnary
                         do symbol <- parseMultiplicationSymbol
                            u2 <- parseUnary
                            return (case symbol of
                                    "*" -> exprMultiply u1 u2
                                    "/" -> exprDivide u1 u2)
                           <|> return u1

parseUnary :: Parser Expr
parseUnary = do symbol <- symbol "-"
                u <- parseUnary
                return (exprNegate u)
               <|> parsePrimary

parsePrimary :: Parser Expr
parsePrimary = parseInt <|> parseParens

parseInt :: Parser Expr
parseInt = do n <- natural
              return (EInt n)

type Constant = String
type Channel = String
type Var = String
type Relabeling = [(Channel, Channel)]

data Process
  = PInaction
  | PConst Constant [Expr] Process
  | PInputPrefix Channel Var Process
  | POutputPrefix Channel Var Process
  | PTauPrefix Process
  | PIfThen Expr Process
  | PParallel Process Process
  | PSum Process Process
  | PRestriction Process [Channel]
  | PRelabeling Process Relabeling
  deriving Show

type Program = [Process]

parseInaction :: Parser Process
parseInaction = do symbol "0"
                   return PInaction

parseExprParens :: Parser [Expr]
parseExprParens = do symbol "("
                     do exprs <- parseExprs
                        symbol ")"
                        return exprs
                       <|> do symbol ")"
                              return []

parseConstant :: Parser Process
parseConstant = do name <- token (many letter)
                   exprs <- parseExprParens
                   symbol "="
                   proc <- parseProcess
                   return (PConst name exprs proc)

parseInputPrefix :: Parser Process
parseInputPrefix = do chan <- token (many letter)
                      symbol "("
                      var <- token (many letter)
                      symbol ")"
                      symbol "."
                      proc <- parseProcess
                      return (PInputPrefix chan var proc)

parseOutputPrefix :: Parser Process
parseOutputPrefix = do symbol "'"
                       chan <- token (many letter)
                       symbol "("
                       var <- token (many letter)
                       symbol ")"
                       symbol "."
                       proc <- parseProcess
                       return (POutputPrefix chan var proc)

parseTauPrefix :: Parser Process
parseTauPrefix = do symbol "tau"
                    symbol "."
                    proc <- parseProcess
                    return (PTauPrefix proc)

parseIfThen :: Parser Process
parseIfThen = do symbol "if"
                 b <- parseBooleanExpr
                 symbol "then"
                 proc <- parseProcess
                 return (PIfThen b proc)

parseParallel :: Parser Process
parseParallel = do p1 <- parseProcess
                   symbol "|"
                   p2 <- parseProcess
                   return (PParallel p1 p2)

parseSum :: Parser Process
parseSum = do p <- parseProcess
              do symbol "+"
                 ps <- parseSum
                 return (PSum p ps)

parseNames :: Parser [String]
parseNames = do n <- token (many letter)
                do symbol ","
                   ns <- parseNames
                   return (n:ns)
                  <|> return [n]

parseSetOfNames :: Parser [String]
parseSetOfNames = do symbol "{"
                     chns <- parseNames
                     symbol "}"
                     return chns

parseSingleName :: Parser [String]
parseSingleName = do chn <- many (token letter)
                     return [chn]

parseRestrictionSet :: Parser [Channel]
parseRestrictionSet = parseSetOfNames <|> parseSingleName

parseRestriction :: Parser Process
parseRestriction = do p <- parseProcess
                      symbol "\\"
                      rest <- parseRestrictionSet
                      return (PRestriction p rest)

parseSingleRelabeling :: Parser (Channel, Channel)
parseSingleRelabeling = do ch1 <- token (many letter)
                           symbol "/"
                           ch2 <- token (many letter)
                           return (ch1, ch2)

parseRelabelingFunction :: Parser Relabeling
parseRelabelingFunction = do r <- parseSingleRelabeling
                             do symbol ","
                                rs <- parseRelabelingFunction
                                return (r:rs)
                               <|> return [r]

parseRelabeling :: Parser Process
parseRelabeling = do p <- parseProcess
                     symbol "["
                     relabeling <- parseRelabelingFunction
                     symbol "]"
                     return (PRelabeling p relabeling)

parseParensProcess :: Parser Process
parseParensProcess = do symbol "("
                        p <- parseProcess
                        symbol ")"
                        return p

parseProcess4 :: Parser Process
parseProcess4 = parseInaction <|> parseInputPrefix <|> parseOutputPrefix <|>
                parseTauPrefix <|> parseIfThen <|> parseParensProcess

parseProcess3 :: Parser Process
parseProcess3 = do p1 <- parseProcess4
                   do symbol "\\"
                      rest <- parseRestrictionSet
                      return (PRestriction p1 rest)
                     <|> do symbol "["
                            relabeling <- parseRelabelingFunction
                            symbol "]"
                            return (PRelabeling p1 relabeling)
                        <|> return p1

parseProcess2 :: Parser Process
parseProcess2 = do p1 <- parseProcess3
                   do symbol "|"
                      p2 <- parseProcess2
                      return (PParallel p1 p2)
                     <|> return p1

parseProcess1 :: Parser Process
parseProcess1 = do p1 <- parseProcess2
                   do symbol "+"
                      p2 <- parseProcess1
                      return (PSum p1 p2)
                     <|> return p1

parseProcess :: Parser Process
parseProcess = parseConstant <|> parseProcess1

-- ### The program consists of a series of processes
-- program -> process1 process2 process3 ...
parseProg :: Parser Program
parseProg = do p <- parseProcess
               do ps <- parseProg
                  return (p:ps)
                <|> return [p]