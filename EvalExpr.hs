module EvalExpr where
import ParseExpressionVar
import Context

-- Expression values should never go outside the universe of values
-- described in the context
checkIntContext :: Int -> Context -> Maybe Int
checkIntContext n c = if intInContext n c then Just n else Nothing
-- This is more useful for integer values but we check booleans as well
-- for uniformity
checkBoolContext :: Bool -> Context -> Maybe Bool
checkBoolContext b c = if boolInContext b c then Just b else Nothing

evalIntExpr :: IntegerExpression -> Context -> Maybe Int
evalIntExpr (EDiv e1 e2) c = do v1 <- evalIntExpr e1 c
                                v2 <- evalIntExpr e2 c
                                r <- checkIntContext (div v1 v2) c
                                return r
evalIntExpr (EMult e1 e2) c = do v1 <- evalIntExpr e1 c
                                 v2 <- evalIntExpr e2 c
                                 r <- checkIntContext (v1 * v2) c
                                 return r
evalIntExpr (ESum e1 e2) c = do v1 <- evalIntExpr e1 c
                                v2 <- evalIntExpr e2 c
                                r <- checkIntContext (v1 + v2) c
                                return r
evalIntExpr (EDiff e1 e2) c = do v1 <- evalIntExpr e1 c
                                 v2 <- evalIntExpr e2 c
                                 r <- checkIntContext (v1 - v2) c
                                 return r
evalIntExpr (ENegate e1) c = do v1 <- evalIntExpr e1 c
                                r <- checkIntContext (negate v1) c
                                return r
evalIntExpr (EIntNat n) c = do r <- checkIntContext n c
                               return r
evalIntExpr (EIntVar v) c = getIntVarValue c v

evalBoolExpr :: BooleanExpression -> Context -> Maybe Bool
evalBoolExpr (EAnd e1 e2) c = do v1 <- evalBoolExpr e1 c
                                 v2 <- evalBoolExpr e2 c
                                 r <- checkBoolContext (v1 && v2) c
                                 return r
evalBoolExpr (EOr e1 e2) c = do v1 <- evalBoolExpr e1 c
                                v2 <- evalBoolExpr e2 c
                                r <- checkBoolContext (v1 || v2) c
                                return r
evalBoolExpr (ENot e1) c = do v1 <- evalBoolExpr e1 c
                              r <- checkBoolContext (not v1) c
                              return r
evalBoolExpr (EBEq e1 e2) c = do v1 <- evalBoolExpr e1 c
                                 v2 <- evalBoolExpr e2 c
                                 r <- checkBoolContext (v1 == v2) c
                                 return r
evalBoolExpr (EBNeq e1 e2) c = do v1 <- evalBoolExpr e1 c
                                  v2 <- evalBoolExpr e2 c
                                  r <- checkBoolContext (v1 /= v2) c
                                  return r
evalBoolExpr (EIEq e1 e2) c = do v1 <- evalIntExpr e1 c
                                 v2 <- evalIntExpr e2 c
                                 r <- checkBoolContext (v1 == v2) c
                                 return r
evalBoolExpr (EINeq e1 e2) c = do v1 <- evalIntExpr e1 c
                                  v2 <- evalIntExpr e2 c
                                  r <- checkBoolContext (v1 /= v2) c
                                  return r
evalBoolExpr (EIGt e1 e2) c = do v1 <- evalIntExpr e1 c
                                 v2 <- evalIntExpr e2 c
                                 r <- checkBoolContext (v1 > v2) c
                                 return r
evalBoolExpr (EIGet e1 e2) c = do v1 <- evalIntExpr e1 c
                                  v2 <- evalIntExpr e2 c
                                  r <- checkBoolContext (v1 >= v2) c
                                  return r
evalBoolExpr (EILt e1 e2) c = do v1 <- evalIntExpr e1 c
                                 v2 <- evalIntExpr e2 c
                                 r <- checkBoolContext (v1 < v2) c
                                 return r
evalBoolExpr (EILet e1 e2) c = do v1 <- evalIntExpr e1 c
                                  v2 <- evalIntExpr e2 c
                                  r <- checkBoolContext (v1 <= v2) c
                                  return r
evalBoolExpr (EBoolValue b) c = do r <- checkBoolContext b c
                                   return r
evalBoolExpr (EBoolVar v) c = getBoolVarValue c v

printExpr :: Expr -> Context -> Maybe String
printExpr (EInt ie) ctx = fmap show (evalIntExpr ie ctx)
printExpr (EBool be) ctx = fmap show (evalBoolExpr be ctx)

printExprs :: [Expr] -> Context -> Maybe String
printExprs [] _ = return ""
printExprs (e:es) ctx = do e1 <- printExpr e ctx
                           en <- printExprs es ctx
                           return (e1 ++ en)