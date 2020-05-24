module Test where
import Parse
import ParseVPCCS
import CompileCCS
import PrettyPrinter
import ParseExpressionVar

compareProgram :: String -> Program -> Bool
compareProgram inp prog = prog == (compileProgram (parse parseProg inp))

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
  , ("P = 0", [Definition "P" Inaction])
    -- Definition with one variable
  , ("P(x) = 0", [Definition "P1" Inaction, Definition "P2" Inaction])
    -- Definition with multiple variables
  , ("P(x,y) = 0", [Definition "P1,1" Inaction, Definition "P1,2" Inaction, Definition "P2,1" Inaction, Definition "P2,2" Inaction])
    -- Input prefix (constant with no variables)
  , ("in(x).K", [Sum (InputPrefix "in1" (Const "K")) (InputPrefix "in2" (Const "K"))])
    -- Input prefix (constant with variables)
  , ("in(x).K(x)", [Sum (InputPrefix "in1" (Const "K1")) (InputPrefix "in2" (Const "K2"))])
    -- Output prefix
  , ("'out(1).0", [OutputPrefix "out1" Inaction])
    -- Input prefix with output prefix
  , ("in(x).'out(x+1).0", [Sum (InputPrefix "in1" (OutputPrefix "out2" Inaction)) (InputPrefix "in2" (OutputPrefix "out3" Inaction))])
    -- Tau prefix
  , ("tau.0", [TauPrefix Inaction])
    -- Parallel processes
  , ("0 | 0", [Parallel Inaction Inaction])
    -- More parallel processes
  , ("0 | 0 | 0", [Parallel Inaction (Parallel Inaction Inaction)])
    -- Complex Parallel example
  , ("(in(x).0) | ('out(2).0)", [Parallel (Sum (InputPrefix "in1" Inaction) (InputPrefix "in2" Inaction)) (OutputPrefix "out2" Inaction)])
    -- Sum of processes
  , ("0 + 0", [Sum Inaction Inaction])
    -- Sum of more processes
  , ("0 + 0 + 0", [Sum Inaction (Sum Inaction Inaction)])
    -- Complex sum example
  , ("('out(1).0) + tau.0", [Sum (OutputPrefix "out1" Inaction) (TauPrefix Inaction)])
    -- Restriction on unused channel
  , ("0\\{p}", [Inaction])
    -- Restriction on input channel
  , ("(in(x).0)\\{in}", [Restriction (Sum (InputPrefix "in1" Inaction) (InputPrefix "in2" Inaction)) ["in1","in2"]])
    -- Restriction on output channel
  , ("('out(3).0)\\{out}", [Restriction (OutputPrefix "out3" Inaction) ["out3"]])
    -- Complex Restriction example
  , ("(in(x).'out(x + 1).0)\\{out,in}", [Restriction (Sum (InputPrefix "in1" (OutputPrefix "out2" Inaction)) (InputPrefix "in2" (OutputPrefix "out3" Inaction))) ["out2","out3","in1","in2"]])
    -- Relabeling on unused channel
  , ("0[newIn/in]", [Inaction])
    -- Relabeling on input channel
  , ("(in(x).0)[newIn/in]", [Relabel (Sum (InputPrefix "in1" Inaction) (InputPrefix "in2" Inaction)) [("newIn1","in1"),("newIn2","in2")]])
    -- Relabeling on output channel
  , ("('out(3).0)[newOut/out]", [Relabel (OutputPrefix "out3" Inaction) [("newOut3","out3")]])
    -- Complex Relabeling example
  , ("(in(x).'out(x+1).0)[newIn/in, newOut/out]", [Relabel (Sum (InputPrefix "in1" (OutputPrefix "out2" Inaction)) (InputPrefix "in2" (OutputPrefix "out3" Inaction))) [("newIn1","in1"),("newIn2","in2"),("newOut2","out2"),("newOut3","out3")]])
  ]

runTests :: IO ()
runTests = testCompiler compilerTests (0, length compilerTests)