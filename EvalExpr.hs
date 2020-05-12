module EvalExpr where
import ParseExpressionVar

type Context = [(String, Int)]

getVarValue :: Context -> String -> Int
getVarValue [] v = error "Variable not found in context"
getVarValue ((w,n):xs) v = if v == w then n else getVarValue xs v

generateVarValues :: [Int] -> String -> Context
generateVarValues [] _ = error "Empty set defined for variable values"
generateVarValues ns v = [(v, i) | i <- ns]

generateContexts :: [Int] -> [String] -> [Context]
generateContexts _ [] = pure []
generateContexts ns (v:vs) = (:) <$> (generateVarValues ns v) <*> (generateContexts ns vs)

evalIntExpr :: IntegerExpression -> Context -> Int
evalIntExpr (EDiv e1 e2) c = (div (evalIntExpr e1 c) (evalIntExpr e2 c))
evalIntExpr (EMult e1 e2) c = (evalIntExpr e1 c) * (evalIntExpr e2 c)
evalIntExpr (ESum e1 e2) c = (evalIntExpr e1 c) + (evalIntExpr e2 c)
evalIntExpr (EDiff e1 e2) c = (evalIntExpr e1 c) - (evalIntExpr e2 c)
evalIntExpr (ENegate e1) c = negate (evalIntExpr e1 c)
evalIntExpr (EIntNat n) _ = n
evalIntExpr (EIntVar v) c = getVarValue c v

evalBoolExpr :: BooleanExpression -> Context -> Bool
evalBoolExpr (EAnd e1 e2) c = (evalBoolExpr e1 c) && (evalBoolExpr e2 c)
evalBoolExpr (EOr e1 e2) c = (evalBoolExpr e1 c) || (evalBoolExpr e2 c)
evalBoolExpr (ENot e1) c = not (evalBoolExpr e1 c)
evalBoolExpr (EBEq e1 e2) c = (evalBoolExpr e1 c) == (evalBoolExpr e2 c)
evalBoolExpr (EBNeq e1 e2) c = (evalBoolExpr e1 c) /= (evalBoolExpr e2 c)
evalBoolExpr (EIEq e1 e2) c = (evalIntExpr e1 c) == (evalIntExpr e2 c)
evalBoolExpr (EINeq e1 e2) c = (evalIntExpr e1 c) /= (evalIntExpr e2 c)
evalBoolExpr (EIGt e1 e2) c = (evalIntExpr e1 c) > (evalIntExpr e2 c)
evalBoolExpr (EIGet e1 e2) c = (evalIntExpr e1 c) >= (evalIntExpr e2 c)
evalBoolExpr (EILt e1 e2) c = (evalIntExpr e1 c) < (evalIntExpr e2 c)
evalBoolExpr (EILet e1 e2) c = (evalIntExpr e1 c) <= (evalIntExpr e2 c)
evalBoolExpr (EBoolValue b) c = b
evalBoolExpr (EBoolVar _) _ = error "Free variable in boolean expression"