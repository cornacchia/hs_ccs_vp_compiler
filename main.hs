import System.IO
import System.Environment
import Parse
import ParseVPCCS
import CompileCCS
import PrettyPrinter

readloop :: Handle -> IO [Char]
readloop inh = do ineof <- hIsEOF inh
                  if ineof
                    then return []
                    else do
                      x <- hGetLine inh
                      xs <- readloop inh
                      return (x ++ " " ++ xs)

readF :: IO String
readF = do args <- getArgs
           inh <- openFile (head args) ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main :: IO()
main = do inp <- readF
          printProgram (compileProgram (parse parseProg inp))