module Test where
import Parse
import PrettyPrinter
import ParseExpressionVar
import EvalExpr
import Context

testContext :: Context
testContext = ([0..], [True, False], [], [])

testExpression :: String -> Expr
testExpression inp = case (parse parseExpression emptyParserContext inp) of
                          [(e, _, _)] -> e
                          _ -> error "No parse"

readEvalPrintExpr :: String -> String
readEvalPrintExpr inp = case printExpr (testExpression inp) testContext of
                             (Just s) -> s
                             _ -> error "No print"

compareExpr :: String -> Expr -> Bool
compareExpr inp ex = ex == (testExpression inp)

testExpr :: (String, Expr) -> IO Bool
testExpr (inp, ex) = if res then do {putStrLn ("OK: " ++ inp ++ " ==> " ++ (readEvalPrintExpr inp)); return True} else do {putStrLn ("!!!Failed!!! ==> " ++ inp); return False}
                        where res = compareExpr inp ex

testExpressions :: [(String, Expr)] -> (Int, Int) -> IO ()
testExpressions [] (n, m) = putStrLn ("Test passed: " ++ (show n) ++ " out of " ++ (show m))
testExpressions (t:ts) (n, m) = do res <- testExpr t
                                   if res then testExpressions ts (n+1, m) else testExpressions ts (n, m)

expressionsTests :: [(String, Expr)]
expressionsTests =
  [ ("0 + 1", EInt (ESum (EIntNat 0) (EIntNat 1)))
  , ("1 * 2 + 3", EInt (ESum (EMult (EIntNat 1) (EIntNat 2)) (EIntNat 3)))
  , ("1 / 2 + 3", EInt (ESum (EDiv (EIntNat 1) (EIntNat 2)) (EIntNat 3)))
  , ("(0 < 3) && True", EBool (EAnd (EILt (EIntNat 0) (EIntNat 3)) (EBoolValue True)))
  , ("(0 < 3) || (4 + 1 > 2)", EBool (EOr (EILt (EIntNat 0) (EIntNat 3)) (EIGt (ESum (EIntNat 4) (EIntNat 1)) (EIntNat 2))))
  ]

runExpressionsTests :: IO ()
runExpressionsTests = testExpressions expressionsTests (0, length expressionsTests)
