module CompileCCS where
import ParseVPCCS
import ParseExpressionVar
import Parse
import EvalExpr

type Def = String
type Constant = String
type Channel = String
type Relabeling = [(Channel, Channel)]

data Process
  = Inaction
  | Definition Def Process
  | Const Constant
  | InputPrefix Channel Process
  | OutputPrefix Channel Process
  | TauPrefix Process
  | Parallel Process Process
  | Sum Process Process
  | Restriction Process [Channel]
  | Relabel Process Relabeling
  deriving Show

type Program = [Process]

-- Define a finite set N of natural numbers
naturals :: [Int]
--naturals = [1..3]
naturals = [1, 2]

showExpression :: Expr -> Context -> String
showExpression (EInt n) ctx = show (evalIntExpr n ctx)
showExpression (EBool b) ctx = show (evalBoolExpr b ctx)

translateExpressions :: [Expr] -> Context -> String
translateExpressions [] _ = []
translateExpressions [x] ctx = showExpression x ctx
translateExpressions (x:xs) ctx = (showExpression x ctx) ++ ", " ++ (translateExpressions xs ctx)

translateContext :: Context -> String
translateContext [] = []
translateContext [x] = show (snd x)
translateContext (x:xs) = show (snd x) ++ "," ++ translateContext xs

compileInputPrefixing :: VP_Process -> Context -> (String, Int) -> (Process, [(Channel, Int)])
compileInputPrefixing (VP_InputPrefix c (EInt ie) p) ctx d = (InputPrefix ch cp, ((c, iv):ex))
                                                     where (cp, ex) = (compileProcess p (d:ctx))
                                                           iv = evalIntExpr ie (d:ctx)
                                                           ch = c ++ (show iv)

compileInputPrefixings :: VP_Process -> Context -> Context -> (Process, [(Channel, Int)])
compileInputPrefixings p ctx [d] = compileInputPrefixing p ctx d
compileInputPrefixings p ctx (d:ds) = (Sum cp1 cp2, ex1 ++ ex2)
                                      where (cp1, ex1) = (compileInputPrefixing p ctx d)
                                            (cp2, ex2) = (compileInputPrefixings p ctx ds)


compileProcess :: VP_Process -> Context -> (Process, [(Channel, Int)])
compileProcess VP_Inaction _ = (Inaction, [])
compileProcess (VP_Constant (c, es)) ctx = (Const (c ++ (translateExpressions es ctx)), [])
compileProcess (VP_TauPrefix p) ctx = (TauPrefix cp, ex)
                                      where (cp, ex) = (compileProcess p ctx)
compileProcess (VP_Sum p1 p2) ctx = (Sum cp1 cp2, ex1 ++ ex2)
                                    where (cp1, ex1) = (compileProcess p1 ctx)
                                          (cp2, ex2) = (compileProcess p2 ctx)
compileProcess (VP_Parallel p1 p2) ctx = (Parallel cp1 cp2, ex1 ++ ex2)
                                         where (cp1, ex1) = (compileProcess p1 ctx)
                                               (cp2, ex2) = (compileProcess p2 ctx)
compileProcess (VP_OutputPrefix c (EInt ie) p) ctx = (OutputPrefix ch cp, ((c, iv):ex))
                                             where (cp, ex) = (compileProcess p ctx)
                                                   iv = evalIntExpr ie ctx
                                                   ch = c ++ (show iv)
compileProcess (VP_Restriction p cs) ctx = if ((length ex) > 0) then (rcp, ex) else (cp, ex)
                                           where (cp, ex) = (compileProcess p ctx)
                                                 rcp = Restriction cp [c ++ show n | c <- cs, (_, n) <- filter ((==c).fst) ex]
compileProcess (VP_Relabel p r) ctx = if ((length ex) > 0) then (rcp, ex) else (cp, ex)
                                      where (cp, ex) = (compileProcess p ctx)
                                            rcp = Relabel cp [(c1 ++ show n, c2 ++ show n) | (c1, c2) <- r, (_, n) <- filter ((==c2).fst) ex]
compileProcess (VP_IfThen (EBool bexpr) p) ctx = if bval then (compileProcess p ctx) else (Inaction, [])
                                                 where bval = (evalBoolExpr bexpr ctx)
compileProcess (VP_IfThen _ _) _ = error "Non boolean expression used as condition in conditional statement (if then else)"
compileProcess (VP_InputPrefix x (EInt (EIntVar v)) p) ctx = compileInputPrefixings (VP_InputPrefix x (EInt (EIntVar v)) p) ctx (concat (generateContexts naturals [v]))
compileProcess (VP_Definition (c, vs) p) ctx = (Definition (c ++ (translateContext ctx)) cp, ex)
                                               where (cp, ex) = (compileProcess p ctx)

compiler :: VP_Program -> Program
compiler [] = []
compiler ((VP_Definition (c, vs) p):ps) = [fst (compileProcess (VP_Definition (c, vs) p) ctx) | ctx <- (generateContexts naturals vs)] ++ (compiler ps)
compiler (p:ps) = (fst (compileProcess p [])) : (compiler ps)

compileProgram :: [(VP_Program, String)] -> Program
compileProgram [(p, "")] = compiler p

test input = compileProgram (parse parseProg input)