module ParseProg where
import Control.Applicative
import Parse
import ParseExpression

parseExprs :: Parser [Expr]
parseExprs = do e <- parseExpression
                do symbol ","
                   es <- parseExprs
                   return (e:es)
                  <|> return [e]

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
                 b <- parseBoolExpr
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