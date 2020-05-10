module ParseVPCCS where
import Control.Applicative
import Parse
import ParseExpression

type VP_Constant = String
type VP_Channel = String
type VP_Var = String
type VP_Relabeling = [(VP_Channel, VP_Channel)]

data VP_Process
  = VP_Inaction
  | VP_Const VP_Constant [Expr] VP_Process
  | VP_InputPrefix VP_Channel VP_Var VP_Process
  | VP_OutputPrefix VP_Channel Expr VP_Process
  | VP_TauPrefix VP_Process
  | VP_IfThen Expr VP_Process
  | VP_Parallel VP_Process VP_Process
  | VP_Sum VP_Process VP_Process
  | VP_Restriction VP_Process [VP_Channel]
  | VP_Relabel VP_Process VP_Relabeling
  deriving Show

type VP_Program = [VP_Process]

parseInaction :: Parser VP_Process
parseInaction = do symbol "0"
                   return VP_Inaction

parseConstant :: Parser VP_Process
parseConstant = do name <- token (many letter)
                   exprs <- parseExprParens
                   symbol "="
                   proc <- parseProcess
                   return (VP_Const name exprs proc)

parseInputPrefix :: Parser VP_Process
parseInputPrefix = do chan <- token (many letter)
                      symbol "("
                      var <- token (many letter)
                      symbol ")"
                      symbol "."
                      proc <- parseProcess
                      return (VP_InputPrefix chan var proc)

parseOutputPrefix :: Parser VP_Process
parseOutputPrefix = do symbol "'"
                       chan <- token (many letter)
                       symbol "("
                       expr <- parseExpression
                       symbol ")"
                       symbol "."
                       proc <- parseProcess
                       return (VP_OutputPrefix chan expr proc)

parseTauPrefix :: Parser VP_Process
parseTauPrefix = do symbol "tau"
                    symbol "."
                    proc <- parseProcess
                    return (VP_TauPrefix proc)

parseIfThen :: Parser VP_Process
parseIfThen = do symbol "if"
                 b <- parseBoolExpr
                 symbol "then"
                 proc <- parseProcess
                 return (VP_IfThen b proc)

parseParallel :: Parser VP_Process
parseParallel = do p1 <- parseProcess
                   symbol "|"
                   p2 <- parseProcess
                   return (VP_Parallel p1 p2)

parseSum :: Parser VP_Process
parseSum = do p <- parseProcess
              do symbol "+"
                 ps <- parseSum
                 return (VP_Sum p ps)

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

parseRestrictionSet :: Parser [VP_Channel]
parseRestrictionSet = parseSetOfNames <|> parseSingleName

parseRestriction :: Parser VP_Process
parseRestriction = do p <- parseProcess
                      symbol "\\"
                      rest <- parseRestrictionSet
                      return (VP_Restriction p rest)

parseSingleRelabeling :: Parser (VP_Channel, VP_Channel)
parseSingleRelabeling = do ch1 <- token (many letter)
                           symbol "/"
                           ch2 <- token (many letter)
                           return (ch1, ch2)

parseRelabelingFunction :: Parser VP_Relabeling
parseRelabelingFunction = do r <- parseSingleRelabeling
                             do symbol ","
                                rs <- parseRelabelingFunction
                                return (r:rs)
                               <|> return [r]

parseRelabeling :: Parser VP_Process
parseRelabeling = do p <- parseProcess
                     symbol "["
                     relabeling <- parseRelabelingFunction
                     symbol "]"
                     return (VP_Relabel p relabeling)

parseParensProcess :: Parser VP_Process
parseParensProcess = do symbol "("
                        p <- parseProcess
                        symbol ")"
                        return p

parseProcess4 :: Parser VP_Process
parseProcess4 = parseInaction <|> parseInputPrefix <|> parseOutputPrefix <|>
                parseTauPrefix <|> parseIfThen <|> parseParensProcess

parseProcess3 :: Parser VP_Process
parseProcess3 = do p1 <- parseProcess4
                   do symbol "\\"
                      rest <- parseRestrictionSet
                      return (VP_Restriction p1 rest)
                     <|> do symbol "["
                            relabeling <- parseRelabelingFunction
                            symbol "]"
                            return (VP_Relabel p1 relabeling)
                        <|> return p1

parseProcess2 :: Parser VP_Process
parseProcess2 = do p1 <- parseProcess3
                   do symbol "|"
                      p2 <- parseProcess2
                      return (VP_Parallel p1 p2)
                     <|> return p1

parseProcess1 :: Parser VP_Process
parseProcess1 = do p1 <- parseProcess2
                   do symbol "+"
                      p2 <- parseProcess1
                      return (VP_Sum p1 p2)
                     <|> return p1

parseProcess :: Parser VP_Process
parseProcess = parseConstant <|> parseProcess1

-- ### The VP_Program consists of a series of VP_Processes
-- VP_Program -> VP_Process1 VP_Process2 VP_Process3 ...
parseProg :: Parser VP_Program
parseProg = do p <- parseProcess
               do ps <- parseProg
                  return (p:ps)
                <|> return [p]