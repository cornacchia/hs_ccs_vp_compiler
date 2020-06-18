module Compile where
import Parse
import ParseVPCCS
import ParseExpressionVar
import EvalExpr
import Context

type Def = String
type Constant = String
type Channel = String
type Relabeling = [(Channel, Channel)]

data Process
  = Inaction
  | Definition Def Bool Process
  | Const Constant
  | InputPrefix Channel Process
  | OutputPrefix Channel Process
  | TauPrefix Process
  | Parallel Process Process
  | Sum Process Process
  | SumIp Process Process
  | Restriction Process [Channel]
  | Relabel Process Relabeling
  deriving (Show, Eq)

type Program = [Process]

chanName :: String -> String -> String
chanName ch val = ch ++ "_" ++ val

bindToStr :: Bindings -> [Expr] -> String
bindToStr b [] = ""
bindToStr b ((EInt (EIntVar v)):es) = show (extractIntBinding v b) ++ bindToStr b es
bindToStr b ((EBool (EBoolVar v)):es) = show (extractBoolBinding v b) ++ bindToStr b es

extractIntChanName :: String -> String -> Bindings -> String
extractIntChanName v ch b = chanName ch (show (extractIntBinding v b))

extractBoolChanName :: String -> String -> Bindings -> String
extractBoolChanName v ch b = chanName ch (show (extractBoolBinding v b))

sumProcesses :: [(Process, Bindings)] -> (Process, Bindings)
sumProcesses [p] = p
sumProcesses ((p,b):ps) = (SumIp p rp, addBindings b rb)
                            where (rp, rb) = sumProcesses ps

compIP :: VP_Process -> Context -> Bindings -> [(Process, Bindings)]
compIP (VP_InputPrefix c (EInt (EIntVar v)) p) ctx b = do (cp, nb) <- comp p (addCtxBindings ctx b)
                                                          return (InputPrefix (extractIntChanName v c b) cp, addIntBindings (c, (extractIntBinding v b) ) nb)
compIP (VP_InputPrefix c (EBool (EBoolVar v)) p) ctx b = do (cp, nb) <- comp p (addCtxBindings ctx b)
                                                            return (InputPrefix (extractBoolChanName v c b) cp, addBoolBindings (c, (extractBoolBinding v b) ) nb)

-- This function receives the usual "global" Context and a list of integer and boolean bindings
compIPs :: VP_Process -> Context -> [Bindings] -> [(Process, Bindings)]
compIPs p ctx [b] = compIP p ctx b
compIPs p ctx bs = case concat (filter (not . null) (map (compIP p ctx) bs)) of
                   [] -> []
                   ps -> [sumProcesses ps]

compDef :: VP_Process -> Context -> Bindings -> [(Process, Bindings)]
compDef (VP_Definition (c, exs) p) ctx b = do (cp, nb) <- comp p (addCtxBindings ctx b)
                                              return (Definition (c ++ (bindToStr b exs)) ((not.null) exs) cp, nb)

compDefns :: VP_Process -> Context -> [Bindings] -> [(Process, Bindings)]
compDefns p ctx [b] = compDef p ctx b
compDefns p ctx bs = concat (filter (not . null) (map (compDef p ctx) bs))

calcNatRestr :: [(String, Int)] -> [VP_Channel] -> [String]
calcNatRestr m cs = [chanName c (show n) | c <- cs, (_, n) <- filter ((==c).fst) m]

calcBoolRestr :: [(String, Bool)] -> [VP_Channel] -> [String]
calcBoolRestr m cs = [chanName c (show b) | c <- cs, (_, b) <- filter ((==c).fst) m]

calcRestrictions :: Bindings -> [VP_Channel] -> [String]
calcRestrictions (nb, bb) cs = calcNatRestr nb cs ++ calcBoolRestr bb cs

calcNatRlbl :: [(String, Int)] -> VP_Relabeling -> Relabeling
calcNatRlbl m r = [(chanName c1 (show n), chanName c2 (show n)) | (c1, c2) <- r, (_, n) <- filter ((==c2).fst) m]

calcBoolRlbl :: [(String, Bool)] -> VP_Relabeling -> Relabeling
calcBoolRlbl m r = [(chanName c1 (show b), chanName c2 (show b)) | (c1, c2) <- r, (_, b) <- filter ((==c2).fst) m]

calcRelabel :: Bindings -> VP_Relabeling -> Relabeling
calcRelabel (nb, bb) r = calcNatRlbl nb r ++ calcBoolRlbl bb r

-- From a single CCS-VP Process we can get one ore more CCS Processes
-- each of which will have a list of bindings (useful for restriction and relabeling)
-- and a list of computed constants (useful for cleaning up unused definitions later)
comp :: VP_Process -> Context -> [(Process, Bindings)]
-- Inaction
comp VP_Inaction ctx = [(Inaction, emptyBindings)]
-- Constant (c: constant, es: expressions)
comp (VP_Constant (c, es)) ctx = case printExprs es ctx of
                                 (Just s) -> [(Const (c ++ s), emptyBindings)]
                                 _ -> []
-- TauPrefix (p: process)
comp (VP_TauPrefix p) ctx = do (cp, nb) <- comp p ctx
                               return (TauPrefix cp, nb)
-- Sum (p1: process, p2: process)
comp (VP_Sum p1 p2) ctx = do (cp1, nb1) <- comp p1 ctx
                             (cp2, nb2) <- comp p2 ctx
                             return (Sum cp1 cp2, addBindings nb1 nb2)
-- Parallel (p1: process, p2: process)
comp (VP_Parallel p1 p2) ctx = do (cp1, nb1) <- comp p1 ctx
                                  (cp2, nb2) <- comp p2 ctx
                                  return (Parallel cp1 cp2, addBindings nb1 nb2)
-- OutputPrefix (c: channel, ie: int expression, p: process)
comp (VP_OutputPrefix c (EInt ie) p) ctx = case iv of
                                              (Just n) -> (do {
                                                (cp, nb) <- comp p (addCtxIntBinding (c, n) ctx)
                                                ; return (OutputPrefix (chanName c (show n)) cp, (addIntBindings (c, n) nb))})
                                              (Nothing) -> []
                                           where iv = evalIntExpr ie ctx
-- OutputPrefix (c: channel, be: bool expression, p: process)
comp (VP_OutputPrefix c (EBool be) p) ctx = case bv of
                                              (Just b) -> (do {(cp, nb) <- comp p (addCtxBoolBinding (c, b) ctx)
                                              ; return (OutputPrefix (chanName c (show b)) cp, (addBoolBindings (c, b) nb))})
                                              (Nothing) -> []
                                           where bv = evalBoolExpr be ctx
-- Restriction (p: process, chs: channels)
comp (VP_Restriction p chs) ctx = do (cp, nb) <- comp p ctx
                                     return (if anyBindings nb then (Restriction cp (calcRestrictions nb chs), nb) else (cp, nb))
-- Relabeling (p: process, r: relabeling)
comp (VP_Relabel p r) ctx = do (cp, nb) <- comp p ctx
                               return (if anyBindings nb then (Relabel cp (calcRelabel nb r), nb) else (cp, nb))
-- IfThen (bexp: boolean expression, p: process)
comp (VP_IfThen (EBool bexpr) p p1) ctx = case bval of
                                       (Just True) -> (comp p ctx)
                                       (Just False) -> (comp p1 ctx)
                                       (Nothing) -> []
                                       where bval = (evalBoolExpr bexpr ctx)
-- InputPrefix (c: channel, v: variable, p: process)
comp (VP_InputPrefix c v p) ctx = compIPs (VP_InputPrefix c v p) ctx (genBindings v ctx)
-- Definition (c: constant, exs: expressions, p: process)
comp (VP_Definition (v, exs) p) ctx = compDefns (VP_Definition (v, exs) p) ctx (generateBindings exs ctx)

getProc :: (Process, Bindings) -> Process
getProc (p, _) = p

compiler :: VP_Program -> Context -> Program
compiler [] _ = []
compiler (p:ps) ctx = cps ++ ccp
                      where cmpr = (comp p ctx)
                            cps = map getProc cmpr
                            ccp = compiler ps ctx

getConstants :: Process -> [Constant]
getConstants Inaction = []
getConstants (Definition _ _ p) = getConstants p
getConstants (Const c) = [c]
getConstants (InputPrefix _ p) = getConstants p
getConstants (OutputPrefix _ p) = getConstants p
getConstants (TauPrefix p) = getConstants p
getConstants (Parallel p1 p2) = getConstants p1 ++ getConstants p2
getConstants (Sum p1 p2) = getConstants p1 ++ getConstants p2
getConstants (SumIp p1 p2) = getConstants p1 ++ getConstants p2
getConstants (Restriction p _) = getConstants p
getConstants (Relabel p _) = getConstants p

getProgramConstants :: Program -> [Constant]
getProgramConstants [] = []
getProgramConstants (p:ps) = getConstants p ++ getProgramConstants ps

getDefinitions :: Program -> [Def]
getDefinitions [] = []
getDefinitions ((Definition name _ _):ps) = name : getDefinitions ps
getDefinitions (p:ps) = getDefinitions ps

checkDefinitions :: Process -> [Def] -> Maybe Process
checkDefinitions (SumIp p1 p2) ds = case checkDefinitions p1 ds of
                                    Just p1 -> case checkDefinitions p2 ds of
                                                Just p2 -> Just (SumIp p1 p2)
                                                Nothing -> Just p1
                                    Nothing -> case checkDefinitions p2 ds of
                                                Just p2 -> Just p2
                                                Nothing -> Nothing
checkDefinitions Inaction ds = Just Inaction
checkDefinitions (Definition d b p) ds = case checkDefinitions p ds of
                                         Just p -> Just (Definition d b p)
                                         Nothing -> Nothing
checkDefinitions (Const c) ds = if elem c ds then Just (Const c) else Nothing
checkDefinitions (InputPrefix c p) ds = case checkDefinitions p ds of
                                        Just p -> Just (InputPrefix c p)
                                        Nothing -> Nothing
checkDefinitions (OutputPrefix c p) ds = case checkDefinitions p ds of
                                         Just p -> Just (OutputPrefix c p)
                                         Nothing -> Nothing
checkDefinitions (TauPrefix p) ds = case checkDefinitions p ds of
                                    Just p -> Just (TauPrefix p)
                                    Nothing -> Nothing
checkDefinitions (Parallel p1 p2) ds = case checkDefinitions p1 ds of
                                       Just p1 -> case checkDefinitions p2 ds of
                                                  Just p2 -> Just (Parallel p1 p2)
                                                  Nothing -> Nothing
                                       Nothing -> Nothing
checkDefinitions (Sum p1 p2) ds = case checkDefinitions p1 ds of
                                       Just p1 -> case checkDefinitions p2 ds of
                                                  Just p2 -> Just (Sum p1 p2)
                                                  Nothing -> Nothing
                                       Nothing -> Nothing
checkDefinitions (Restriction p r) ds = case checkDefinitions p ds of
                                        Just p -> Just (Restriction p r)
                                        Nothing -> Nothing
checkDefinitions (Relabel p r) ds = case checkDefinitions p ds of
                                         Just p -> Just (Relabel p r)
                                         Nothing -> Nothing

cleanProgramDefinitions :: Program -> [Def] -> Program
cleanProgramDefinitions [] _ = []
cleanProgramDefinitions (p:ps) d = case checkDefinitions p d of
                                   Just p -> p : cleanProgramDefinitions ps d
                                   Nothing -> cleanProgramDefinitions ps d

cleanProgramConstantsIteration :: Program -> [Constant] -> (Program, [Process])
-- Only check definitions which have expressions attached
cleanProgramConstantsIteration ((Definition name True p):ps) const = if elem name const then ((Definition name True p):rest, rmps) else (rest, (Definition name True p):rmps)
                                                                     where (rest, rmps) = cleanProgramConstantsIteration ps const
cleanProgramConstantsIteration (p:ps) const = ((p:cps), rmps)
                                              where (cps, rmps) = cleanProgramConstantsIteration ps const
cleanProgramConstantsIteration [] _ = ([],[])

cleanProgramConstants :: Program -> Program
cleanProgramConstants [] = []
cleanProgramConstants p = case cleanProgramConstantsIteration p (getProgramConstants p) of
                          (cp, []) -> cp
                          (cp, (x:xs)) -> cleanProgramConstants cp

compileProgram :: [(VP_Program, String, ParserContext)] -> Context -> Program
compileProgram [(p, "", _)] ctx = cleanProgramConstants progIp
                                  where prog = compiler p ctx
                                        progIp = cleanProgramDefinitions prog (getDefinitions prog)
compileProgram [(p, s, _)] _ = error ("Non-exhaustive parse: " ++ s)

-- For tests we avoid some cleanup functions to better show how the compiler works
compileProgramForTests :: [(VP_Program, String, ParserContext)] -> Context -> Program
compileProgramForTests [(p, "", _)] ctx = cleanProgramConstants (compiler p ctx)
compileProgramForTests [(p, s, _)] _ = error ("Non-exhaustive parse: " ++ s)
