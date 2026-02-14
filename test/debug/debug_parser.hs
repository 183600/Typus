import qualified Parser as P
import qualified Compiler as C

main :: IO ()
main = do
  -- Test empty input
  case P.parseTypus "" of
    Left err -> putStrLn $ "Empty input correctly failed: " ++ err
    Right _ -> putStrLn "ERROR: Empty input should have failed!"
  
  -- Test whitespace only input
  case P.parseTypus "   \n\t  " of
    Left err -> putStrLn $ "Whitespace input correctly failed: " ++ err
    Right _ -> putStrLn "ERROR: Whitespace input should have failed!"
  
  -- Test valid input
  let validInput = "package main\nfunc main() {}"
  case P.parseTypus validInput of
    Left err -> putStrLn $ "Valid input failed: " ++ err
    Right parsed -> do
      putStrLn "Valid input parsed successfully"
      case C.compile parsed of
        Left errs -> putStrLn $ "Compilation failed: " ++ show errs
        Right _ -> putStrLn "Compilation succeeded"