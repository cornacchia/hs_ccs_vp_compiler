module PrettyPrinter where
import Compile

translateSingleRelabeling :: (Channel, Channel) -> String
translateSingleRelabeling (c1, c2) = c1 ++ "/" ++ c2

translateRelabeling :: Relabeling -> String
translateRelabeling r = "{" ++ (concat [(translateSingleRelabeling e) ++ ", " | e <- init r]) ++ (translateSingleRelabeling (last r)) ++ "}"

translateRestriction :: [Channel] -> String
translateRestriction xs = "{" ++ (concat [c ++ "," | c <- init xs]) ++ last xs ++ "}"

translateProcess :: Process -> String
translateProcess Inaction = "0"
translateProcess (Definition d _ p) = d ++ " = " ++ (translateProcess p)
translateProcess (Const c) = c
translateProcess (InputPrefix c p) = c ++ ". (" ++ (translateProcess p) ++ ")"
translateProcess (OutputPrefix c p) = "'" ++ c ++ "." ++ (translateProcess p)
translateProcess (TauPrefix p) = "tau." ++ (translateProcess p)
translateProcess (Parallel p1 p2) = "(" ++ (translateProcess p1) ++ ") | (" ++ (translateProcess p2) ++ ")"
translateProcess (Sum p1 p2) = "(" ++ (translateProcess p1) ++ ") + (" ++ (translateProcess p2) ++ ")"
translateProcess (Restriction p cs) = "(" ++ (translateProcess p) ++ ")\\" ++ (translateRestriction cs)
translateProcess (Relabel p r) = "(" ++ (translateProcess p) ++ ")" ++ (translateRelabeling r)

translateProgram :: Program -> [String]
translateProgram (p:ps) = translateProcess p : translateProgram ps
translateProgram [] = []

printProgramCompact :: Program -> String
printProgramCompact p = head progStrings ++ foldl (\xs x -> xs ++ ";" ++ x) "" (tail progStrings)
                        where progStrings = translateProgram p

printProgram :: Program -> IO()
printProgram p = mapM_ putStrLn (translateProgram p)
