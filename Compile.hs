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
  | Restriction Process [Channel]
  | Relabel Process Relabeling
  deriving (Show, Eq)

type Program = [Process]

chanName :: String -> String -> String
chanName ch val = ch ++ "_" ++ val

defIntName :: [(String, Int)] -> String
defIntName [] = ""
defIntName ((v, n):bs) = (show n) ++ defIntName bs

defBoolName :: [(String, Bool)] -> String
defBoolName [] = ""
defBoolName ((v, b):bs) = (show b) ++ defBoolName bs

defName :: String -> Bindings -> String
defName c ([], []) = c
defName c (nb, []) = c ++ (defIntName nb)
defName c ([], bb) = c ++ (defBoolName bb)
defName c (nb, bb) = c ++ ((defIntName nb) ++ (defBoolName bb))

extractIntChanName :: String -> String -> Bindings -> String
extractIntChanName v ch b = chanName ch (show (extractIntBinding v b))

extractBoolChanName :: String -> String -> Bindings -> String
extractBoolChanName v ch b = chanName ch (show (extractBoolBinding v b))

sumProcesses :: [(Process, Bindings, [Constant])] -> (Process, Bindings, [Constant])
sumProcesses [p] = p
sumProcesses ((p,b,c):ps) = (Sum p rp, addBindings b rb, c ++ rc)
                            where (rp, rb, rc) = sumProcesses ps

compIP :: VP_Process -> Context -> Bindings -> [(Process, Bindings, [Constant])]
compIP (VP_InputPrefix c (EInt (EIntVar v)) p) ctx b = do (cp, nb, cs) <- comp p (addCtxBindings ctx b)
                                                          return (InputPrefix (extractIntChanName v c b) cp, addIntBindings (c, (extractIntBinding v b) ) nb, cs)
compIP (VP_InputPrefix c (EBool (EBoolVar v)) p) ctx b = do (cp, nb, cs) <- comp p (addCtxBindings ctx b)
                                                            return (InputPrefix (extractBoolChanName v c b) cp, addBoolBindings (c, (extractBoolBinding v b) ) nb, cs)

-- This function receives the usual "global" Context and a list of integer and boolean bindings
compIPs :: VP_Process -> Context -> [Bindings] -> [(Process, Bindings, [Constant])]
compIPs p ctx [b] = compIP p ctx b
compIPs p ctx bs = case concat (filter (not . null) (map (compIP p ctx) bs)) of
                   [] -> []
                   ps -> [sumProcesses ps]

compDef :: VP_Process -> Context -> Bindings -> [(Process, Bindings, [Constant])]
compDef (VP_Definition (c, exs) p) ctx b = do (cp, nb, cs) <- comp p (addCtxBindings ctx b)
                                              return (Definition (defName c b) ((not.null) exs) cp, nb, cs)

compDefns :: VP_Process -> Context -> [Bindings] -> [(Process, Bindings, [Constant])]
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
comp :: VP_Process -> Context -> [(Process, Bindings, [Constant])]
-- Inaction
comp VP_Inaction ctx = [(Inaction, emptyBindings, [])]
-- Constant (c: constant, es: expressions)
comp (VP_Constant (c, es)) ctx = case printExprs es ctx of
                                 (Just s) -> [(Const (c ++ s), emptyBindings, [c ++ s])]
                                 _ -> []
-- TauPrefix (p: process)
comp (VP_TauPrefix p) ctx = do (cp, nb, cs) <- comp p ctx
                               return (TauPrefix cp, nb, cs)
-- Sum (p1: process, p2: process)
comp (VP_Sum p1 p2) ctx = do (cp1, nb1, cs1) <- comp p1 ctx
                             (cp2, nb2, cs2) <- comp p2 ctx
                             return (Sum cp1 cp2, addBindings nb1 nb2 , cs1 ++ cs2)
-- Parallel (p1: process, p2: process)
comp (VP_Parallel p1 p2) ctx = do (cp1, nb1, cs1) <- comp p1 ctx
                                  (cp2, nb2, cs2) <- comp p2 ctx
                                  return (Parallel cp1 cp2, addBindings nb1 nb2 , cs1 ++ cs2)
-- OutputPrefix (c: channel, ie: int expression, p: process)
comp (VP_OutputPrefix c (EInt ie) p) ctx = case iv of
                                              (Just n) -> (do {
                                                (cp, nb, cs) <- comp p (addCtxIntBinding (c, n) ctx)
                                                ; return (OutputPrefix (chanName c (show n)) cp, (addIntBindings (c, n) nb), cs)})
                                              (Nothing) -> []
                                           where iv = evalIntExpr ie ctx
-- OutputPrefix (c: channel, be: bool expression, p: process)
comp (VP_OutputPrefix c (EBool be) p) ctx = case bv of
                                              (Just b) -> (do {(cp, nb, cs) <- comp p (addCtxBoolBinding (c, b) ctx)
                                              ; return (OutputPrefix (chanName c (show b)) cp, (addBoolBindings (c, b) nb), cs)})
                                              (Nothing) -> []
                                           where bv = evalBoolExpr be ctx
-- Restriction (p: process, chs: channels)
comp (VP_Restriction p chs) ctx = do (cp, nb, cs) <- comp p ctx
                                     return (if anyBindings nb then (Restriction cp (calcRestrictions nb chs), nb, cs) else (cp, nb, cs))
-- Relabeling (p: process, r: relabeling)
comp (VP_Relabel p r) ctx = do (cp, nb, cs) <- comp p ctx
                               return (if anyBindings nb then (Relabel cp (calcRelabel nb r), nb, cs) else (cp, nb, cs))
-- IfThen (bexp: boolean expression, p: process)
comp (VP_IfThen (EBool bexpr) p) ctx = case bval of
                                       (Just True) -> (comp p ctx)
                                       (Just False) -> [(Inaction, emptyBindings, [])]
                                       (Nothing) -> []
                                       where bval = (evalBoolExpr bexpr ctx)
-- InputPrefix (c: channel, v: variable, p: process)
comp (VP_InputPrefix c v p) ctx = compIPs (VP_InputPrefix c v p) ctx (genBindings v ctx)
-- Definition (c: constant, exs: expressions, p: process)
comp (VP_Definition (v, exs) p) ctx = compDefns (VP_Definition (v, exs) p) ctx (generateBindings exs ctx)

getProc :: (Process, Bindings, [Constant]) -> Process
getProc (p, _, _) = p

getConst :: (Process, Bindings, [Constant]) -> [Constant]
getConst (_, _, c) = c

compiler :: VP_Program -> Context -> (Program, [Constant])
compiler [] _ = ([], [])
compiler (p:ps) ctx = (cps ++ ccp, csts ++ ccs)
                      where cmpr = (comp p ctx)
                            cps = map getProc cmpr
                            csts = concat (map getConst cmpr)
                            (ccp, ccs) = compiler ps ctx

cleanProgram :: Program -> [Constant] -> Program
-- Only check definitions which have expressions attached
cleanProgram ((Definition name True p):ps) const = if elem name const then (Definition name True p):rest else rest
                                                   where rest = cleanProgram ps const
cleanProgram (p:ps) const = p:(cleanProgram ps const)
cleanProgram [] _ = []

compileProgram :: [(VP_Program, String, ParserContext)] -> Context -> Program
compileProgram [(p, "", _)] ctx = cleanProgram prog const
                                  where (prog, const) = compiler p ctx
compileProgram [(p, s, _)] _ = error ("Non-exhaustive parse: " ++ s)
