module CompileCCS where
import ParseVPCCS
import ParseExpression

type Constant = String
type Channel = String
type Relabeling = [(Channel, Channel)]

data Process
  = Inaction
  | Const Constant Process
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
naturals = [1..3]

compileSingleInputPrefix :: Int -> VP_Process -> Process
compileSingleInputPrefix n (VP_InputPrefix c v p) = InputPrefix (c ++ show n) (compileProcess (VP_Relabel p [(show n, v)]))

compileInputPrefixing :: [Int] -> VP_Process -> Process
compileInputPrefixing [n] p = compileSingleInputPrefix n p
compileInputPrefixing (n:ns) p = Sum (compileSingleInputPrefix n p) (compileInputPrefixing ns p)

showExpression :: Expr -> String
showExpression (EInt n) = show n
showExpression (EBool b) = show b

translateExpressions :: [Expr] -> String
translateExpressions [] = []
translateExpressions [x] = showExpression x
translateExpressions (x:xs) = (showExpression x) ++ ", " ++ (translateExpressions xs)

compileProcess :: VP_Process -> Process
compileProcess VP_Inaction = Inaction
compileProcess (VP_TauPrefix p) = TauPrefix (compileProcess p)
compileProcess (VP_Sum p1 p2) = Sum (compileProcess p1) (compileProcess p2)
compileProcess (VP_Parallel p1 p2) = Parallel (compileProcess p1) (compileProcess p2)
compileProcess (VP_OutputPrefix c e p) = OutputPrefix (c ++ showExpression e) (compileProcess p)
compileProcess (VP_Restriction p cs) = Restriction (compileProcess p) [c ++ show n | c <- cs, n <- naturals]
compileProcess (VP_Relabel p r) = Relabel (compileProcess p) [(c1 ++ show n, c2 ++ show n) | (c1, c2) <- r, n <- naturals]
compileProcess (VP_IfThen (EBool True) p) = (compileProcess p)
compileProcess (VP_IfThen _ p) = Inaction
compileProcess (VP_InputPrefix x v p) = compileInputPrefixing naturals (VP_InputPrefix x v p)
compileProcess (VP_Const c es p) = Const (c ++ translateExpressions es) (compileProcess (VP_Relabel p [(show e, "x" ++ show i) | (e, i) <- zip es [1..]]))

compiler :: VP_Program -> Program
compiler = map compileProcess

compileProgram :: [(VP_Program, String)] -> Program
compileProgram [(p, "")] = compiler p