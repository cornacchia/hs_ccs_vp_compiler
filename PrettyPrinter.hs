module PrettyPrinter where
import CompileCCS

translateSingleRelabeling :: (Channel, Channel) -> String
translateSingleRelabeling (c1, c2) = c1 ++ "/" ++ c2

translateRelabeling :: Relabeling -> String
translateRelabeling r = "{" ++ (concat [(translateSingleRelabeling e) ++ ", " | e <- init r]) ++ (translateSingleRelabeling (last r)) ++ "}"

translateRestriction :: [Channel] -> String
translateRestriction xs = "{" ++ (concat [c ++ "," | c <- init xs]) ++ last xs ++ "}"

translateProcess :: Process -> String
translateProcess Inaction = "0"
translateProcess (Const c p) = c ++ " = " ++ (translateProcess p)
translateProcess (InputPrefix c p) = c ++ "." ++ (translateProcess p)
translateProcess (OutputPrefix c p) = "'" ++ c ++ "." ++ (translateProcess p)
translateProcess (TauPrefix p) = "tau." ++ (translateProcess p)
translateProcess (Parallel p1 p2) = (translateProcess p1) ++ " | " ++ (translateProcess p2)
translateProcess (Sum p1 p2) = (translateProcess p1) ++ " + " ++ (translateProcess p2)
translateProcess (Restriction p cs) = (translateProcess p) ++ "\\" ++ (translateRestriction cs)
translateProcess (Relabel p r) = (translateProcess p) ++ (translateRelabeling r)

printProcess = putStrLn . translateProcess

printProgram [] = putStrLn ""
printProgram (p:ps) = do printProcess p
                         printProgram ps
