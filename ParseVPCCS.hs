module ParseVPCCS where
import Control.Applicative
import Parse
import ParseExpressionVar

type VP_Def = (String, [Expr])
type VP_Const = (String, [Expr])
type VP_Channel = String
type VP_Var = String
type VP_Relabeling = [(VP_Channel, VP_Channel)]

data VP_Process
  = VP_Inaction
  | VP_Definition VP_Def VP_Process
  | VP_Constant VP_Const
  | VP_InputPrefix VP_Channel Expr VP_Process
  | VP_OutputPrefix VP_Channel Expr VP_Process
  | VP_TauPrefix VP_Process
  | VP_IfThen Expr VP_Process VP_Process
  | VP_Parallel VP_Process VP_Process
  | VP_Sum VP_Process VP_Process
  | VP_Restriction VP_Process [VP_Channel]
  | VP_Relabel VP_Process VP_Relabeling
  deriving (Show, Eq)

type VP_Program = [VP_Process]

parseInaction :: Parser VP_Process
parseInaction = do symbol "0"
                   return VP_Inaction

parseTypedVar :: Parser Expr
parseTypedVar = do v <- parseName
                   symbol ":"
                   do symbol "Bool"
                      updateParserContext v "Bool" (return (EBool (EBoolVar v)))
                    <|> do symbol "Int"
                           updateParserContext v "Int" (return (EInt (EIntVar v)))


parseVars :: Parser [Expr]
parseVars = do v <- parseTypedVar
               do symbol ","
                  vs <- parseVars
                  return (v:vs)
                 <|> return [v]

parseVarParens :: Parser [Expr]
parseVarParens = do symbol "("
                    do vars <- parseVars
                       symbol ")"
                       return vars
                      <|> do symbol ")"
                             return []
                   <|> return []

parseDefinition :: Parser VP_Process
parseDefinition = do const <- parseName
                     vars <- parseVarParens
                     symbol "="
                     proc <- parseProcess
                     return (VP_Definition (const, vars) proc)

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else"]

parseName :: Parser String
parseName = do v <- token variable
               if elem v reservedKeywords then empty else return v

parseConstant :: Parser VP_Process
parseConstant = do name <- parseName
                   do exprs <- parseExprParens
                      return (VP_Constant (name, exprs))
                     <|> do return (VP_Constant (name, []))

parseInputPrefix :: Parser VP_Process
parseInputPrefix = do chan <- parseName
                      symbol "("
                      var <- parseTypedVar
                      symbol ")"
                      symbol "."
                      proc <- parseProcess
                      return (VP_InputPrefix chan var proc)

parseOutputPrefix :: Parser VP_Process
parseOutputPrefix = do symbol "'"
                       chan <- parseName
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
                 do symbol "else"
                    procElse <- parseProcess
                    return (VP_IfThen b proc procElse)
                  <|> return (VP_IfThen b proc VP_Inaction)

parseNames :: Parser [String]
parseNames = do n <- parseName
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
parseSingleName = do chn <- parseName
                     return [chn]

parseRestrictionSet :: Parser [VP_Channel]
parseRestrictionSet = parseSetOfNames <|> parseSingleName

parseRestriction :: Parser VP_Process
parseRestriction = do p <- parseProcess
                      symbol "\\"
                      rest <- parseRestrictionSet
                      return (VP_Restriction p rest)

parseSingleRelabeling :: Parser (VP_Channel, VP_Channel)
parseSingleRelabeling = do ch1 <- parseName
                           symbol "/"
                           ch2 <- parseName
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
                parseTauPrefix <|> parseIfThen <|> parseConstant <|> parseParensProcess

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
parseProcess = parseDefinition <|> parseProcess1

-- ### The VP_Program consists of a series of VP_Processes
-- VP_Program -> VP_Process1 VP_Process2 VP_Process3 ...
parseProg :: Parser VP_Program
parseProg = do p <- parseProcess
               do ps <- parseProg
                  return (p:ps)
                <|> return [p]