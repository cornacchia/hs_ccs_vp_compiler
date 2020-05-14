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
translateContext (x:xs) = show (snd x) ++ "," ++ translateContext xs

compileInputPrefixing :: VP_Process -> Context -> (String, Int) -> Process
compileInputPrefixing (VP_InputPrefix c e p) ctx d = InputPrefix (c ++ (showExpression e (d:ctx))) (compileProcess p (d:ctx))

compileInputPrefixings :: VP_Process -> Context -> Context -> Process
compileInputPrefixings p ctx [d] = compileInputPrefixing p ctx d
compileInputPrefixings p ctx (d:ds) = Sum (compileInputPrefixing p ctx d) (compileInputPrefixings p ctx ds)

compileProcess :: VP_Process -> Context -> Process
compileProcess VP_Inaction _ = Inaction
compileProcess (VP_Constant (c, es)) ctx = Const (c ++ (translateExpressions es ctx))
compileProcess (VP_TauPrefix p) ctx = TauPrefix (compileProcess p ctx)
compileProcess (VP_Sum p1 p2) ctx = Sum (compileProcess p1 ctx) (compileProcess p2 ctx)
compileProcess (VP_Parallel p1 p2) ctx = Parallel (compileProcess p1 ctx) (compileProcess p2 ctx)
compileProcess (VP_OutputPrefix c e p) ctx = OutputPrefix (c ++ (showExpression e ctx)) (compileProcess p ctx)
--compileProcess (VP_Restriction p cs) = Restriction (compileProcess p) [c ++ show n | c <- cs, n <- naturals]
--compileProcess (VP_Relabel p r) = Relabel (compileProcess p) [(c1 ++ show n, c2 ++ show n) | (c1, c2) <- r, n <- naturals]
compileProcess (VP_IfThen (EBool bexpr) p) ctx = if (evalBoolExpr bexpr ctx) then (compileProcess p ctx) else Inaction
compileProcess (VP_IfThen _ _) _ = error "Non boolean expression used as condition in conditional statement (if then else)"
compileProcess (VP_InputPrefix x (EInt (EIntVar v)) p) ctx = compileInputPrefixings (VP_InputPrefix x (EInt (EIntVar v)) p) ctx (concat (generateContexts naturals [v]))
compileProcess (VP_Definition (c, vs) p) ctx = Definition (c ++ (translateContext ctx)) (compileProcess p ctx)

compiler :: VP_Program -> Program
compiler [] = []
compiler ((VP_Definition (c, vs) p):ps) = [compileProcess (VP_Definition (c, vs) p) ctx | ctx <- (generateContexts naturals vs)] ++ (compiler ps)
compiler (p:ps) = (compileProcess p []) : (compiler ps)

compileProgram :: [(VP_Program, String)] -> Program
compileProgram [(p, "")] = compiler p

test input = compileProgram (parse parseProg input)