import Parser (parseTypus)
import qualified Compiler.IR as IR (rawSourceFromTypus)
import Data.List (isInfixOf)

main :: IO ()
main = do
  let content = "var x int = \"string\""
  putStrLn $ "Input: " ++ show content
  
  case parseTypus content of
    Left err -> do
      putStrLn $ "Parse error: " ++ err
    Right typusFile -> do
      putStrLn "Parse successful"
      let rawSource = IR.rawSourceFromTypus typusFile
      putStrLn $ "Raw source: " ++ show rawSource
      putStrLn $ "Contains pattern: " ++ show ("var x int = \"string\"" `isInfixOf` rawSource)