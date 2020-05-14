import System.IO
import Parse
import ParseVPCCS
import CompileCCS
import PrettyPrinter

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                    then return []
                    else do
                      x <- hGetLine inh
                      xs <- readloop inh
                      return (x ++ " " ++ xs)

readF :: IO String
readF = do inh <- openFile "./test/input1" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main = do inp <- readF
          printProgram (compileProgram (parse parseProg inp))