import DependentTypesParser (runDependentTypesParser, validateDependentTypeSyntax)
import System.IO (readFile)

main :: IO ()
main = do
  content <- readFile "test/data/type_system_valid.typus"
  putStrLn "=== Running validateDependentTypeSyntax ==="
  let errors = validateDependentTypeSyntax content
  putStrLn $ "Number of errors: " ++ show (length errors)
  mapM_ print errors
  putStrLn "\n=== Running runDependentTypesParser ==="
  case runDependentTypesParser content of
    Left err -> putStrLn $ "Parser failed: " ++ err
    Right (defs, state) -> do
      putStrLn $ "Parsed " ++ show (length defs) ++ " definitions"
      putStrLn $ "Errors: " ++ show (length (parserErrors state))
      mapM_ print (parserErrors state)
