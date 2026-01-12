import qualified Data.Text as T
import Parser (parseTypus)
import Compiler (compile)
import Types (CompilerError(..), message, ceError)

main :: IO ()
main = do
  let content = "var x int = \"string\""
  putStrLn $ "Input: " ++ show content
  
  case parseTypus content of
    Left err -> do
      putStrLn $ "Parse error: " ++ err
    Right typusFile -> do
      putStrLn "Parse successful"
      
      case compile typusFile of
        Left errs -> do
          putStrLn $ "Compile errors: " ++ show (length errs)
          mapM_ (\e -> putStrLn $ "  Error: " ++ T.unpack (message (ceError e))) errs
        Right result -> do
          putStrLn $ "Compile successful: " ++ show result