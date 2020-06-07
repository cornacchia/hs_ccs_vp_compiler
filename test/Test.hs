module Test where
import Parse
import ParseVPCCS
import Compile
import PrettyPrinter
import ParseExpressionVar
import Context

testContext :: Context
testContext = ([0..2], [True, False], [], [])

testCompile :: String -> Program
testCompile inp = (compileProgram (parse parseProg emptyParserContext inp) testContext)

compareProgram :: String -> Program -> Bool
compareProgram inp prog = prog == (testCompile inp)

test :: (String, Program) -> IO Bool
test (inp, prog) = if res then do {putStrLn ("OK: [[ " ++ inp ++ " ]] ==> " ++ (printProgramCompact prog)); return True} else do {putStrLn ("!!!Failed!!! ==> " ++ inp); return False}
                        where res = compareProgram inp prog

testCompiler :: [(String, Program)] -> (Int, Int) -> IO ()
testCompiler [] (n, m) = putStrLn ("Test passed: " ++ (show n) ++ " out of " ++ (show m))
testCompiler (t:ts) (n, m) = do res <- test t
                                if res then testCompiler ts (n+1, m) else testCompiler ts (n, m)

compilerTests :: [(String, Program)]
compilerTests =
  [ ("0", [Inaction])
    -- Definition without variables
  , ("P = 0", [Definition "P" False Inaction])
    -- Definition with one variable
  , ("P = P(1) P(x:Int) = 0", [Definition "P" False (Const "P1"),Definition "P1" True Inaction])
    -- Definition with multiple variables
  , ("P = P(1,2) P(x:Int,y:Int) = 0", [Definition "P" False (Const "P12"),Definition "P12" True Inaction])
    -- Input prefix (constant with no variables)
  , ("in(x : Int).K", [Sum (InputPrefix "in_0" (Const "K")) (Sum (InputPrefix "in_1" (Const "K")) (InputPrefix "in_2" (Const "K")))])
    -- Input prefix (constant with variables)
  , ("in(x : Int).K(x)", [Sum (InputPrefix "in_0" (Const "K0")) (Sum (InputPrefix "in_1" (Const "K1")) (InputPrefix "in_2" (Const "K2")))])
    -- Input prefix on same variable
  , ("in(x : Int).in(x : Int).0", [Sum (InputPrefix "in_0" (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction)))) (Sum (InputPrefix "in_1" (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction)))) (InputPrefix "in_2" (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction)))))])
    -- Output prefix
  , ("'out(1).0", [OutputPrefix "out_1" Inaction])
    -- Input prefix with output prefix
  , ("in(x : Int).'out(x+1).0", [Sum (InputPrefix "in_0" (OutputPrefix "out_1" Inaction)) (InputPrefix "in_1" (OutputPrefix "out_2" Inaction))])
    -- Multiple input prefix with output prefix
  , ("in(x : Int).in(y : Int).'out(x + y).0", [Sum (InputPrefix "in_0" (Sum (InputPrefix "in_0" (OutputPrefix "out_0" Inaction)) (Sum (InputPrefix "in_1" (OutputPrefix "out_1" Inaction)) (InputPrefix "in_2" (OutputPrefix "out_2" Inaction))))) (Sum (InputPrefix "in_1" (Sum (InputPrefix "in_0" (OutputPrefix "out_1" Inaction)) (InputPrefix "in_1" (OutputPrefix "out_2" Inaction)))) (InputPrefix "in_2" (InputPrefix "in_0" (OutputPrefix "out_2" Inaction))))])
    -- Tau prefix
  , ("tau.0", [TauPrefix Inaction])
    -- Parallel processes
  , ("0 | 0", [Parallel Inaction Inaction])
    -- More parallel processes
  , ("0 | 0 | 0", [Parallel Inaction (Parallel Inaction Inaction)])
    -- Complex Parallel example
  , ("(in(x : Int).0) | ('out(2).0)", [Parallel (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction))) (OutputPrefix "out_2" Inaction)])
    -- Sum of processes
  , ("0 + 0", [Sum Inaction Inaction])
    -- Sum of more processes
  , ("0 + 0 + 0", [Sum Inaction (Sum Inaction Inaction)])
    -- Complex sum example
  , ("('out(1).0) + tau.0", [Sum (OutputPrefix "out_1" Inaction) (TauPrefix Inaction)])
    -- Restriction on unused channel
  , ("0\\{p}", [Inaction])
    -- Restriction on input channel
  , ("(in(x : Int).0)\\{in}", [Restriction (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction))) ["in_0","in_1","in_2"]])
    -- Restriction on output channel
  , ("('out(2).0)\\{out}", [Restriction (OutputPrefix "out_2" Inaction) ["out_2"]])
    -- Complex Restriction example
  , ("(in(x : Int).'out(x + 1).0)\\{out,in}", [Restriction (Sum (InputPrefix "in_0" (OutputPrefix "out_1" Inaction)) (InputPrefix "in_1" (OutputPrefix "out_2" Inaction))) ["out_1","out_2","in_0","in_1"]])
    -- Relabeling on unused channel
  , ("0[newIn/in]", [Inaction])
    -- Relabeling on input channel
  , ("(in(x : Int).0)[newIn/in]", [Relabel (Sum (InputPrefix "in_0" Inaction) (Sum (InputPrefix "in_1" Inaction) (InputPrefix "in_2" Inaction))) [("newIn_0","in_0"),("newIn_1","in_1"),("newIn_2","in_2")]])
    -- Relabeling on output channel
  , ("('out(2).0)[newOut/out]", [Relabel (OutputPrefix "out_2" Inaction) [("newOut_2","out_2")]])
    -- Complex Relabeling example
  , ("(in(x : Int).'out(x+1).0)[newIn/in, newOut/out]", [Relabel (Sum (InputPrefix "in_0" (OutputPrefix "out_1" Inaction)) (InputPrefix "in_1" (OutputPrefix "out_2" Inaction))) [("newIn_0","in_0"),("newIn_1","in_1"),("newOut_1","out_1"),("newOut_2","out_2")]])
  ]

runTests :: IO ()
runTests = testCompiler compilerTests (0, length compilerTests)