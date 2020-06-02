import System.IO
import System.Environment
import qualified Data.Map.Strict as Map
import Parse
import ParseVPCCS
import Compile
import Context
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
readF = do putStrLn "Type the path of the CCS-VP source file to compile and press ENTER:"
           path <- getLine
           inh <- openFile path ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main :: IO()
main = do inp <- readF
          printProgram (compileProgram (parse parseProg (Map.fromList []) inp) testContext)