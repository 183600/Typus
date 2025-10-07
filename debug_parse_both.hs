import Ownership (lexAll, parseProgram)

main :: IO ()
main = do
  -- Test with a simple identifier that should be parsed correctly
  let testCode1 = "x := 1; y := x; z := x"
  putStrLn $ "Testing simple code: " ++ testCode1
  let tokens1 = lexAll testCode1
  putStrLn $ "Tokens: " ++ show tokens1
  putStrLn $ "Parsed: " ++ show (parseProgram tokens1)
  putStrLn $ "---"
  
  -- Test with the problematic slice literal
  let testCode2 = "x := []int{1}; y := x; z := x"
  putStrLn $ "Testing slice literal code: " ++ testCode2
  let tokens2 = lexAll testCode2
  putStrLn $ "Tokens: " ++ show tokens2
  putStrLn $ "Parsed: " ++ show (parseProgram tokens2)