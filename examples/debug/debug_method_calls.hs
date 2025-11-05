import Ownership

main :: IO ()
main = do
  -- Test parsing of method calls
  let code = "mu.Lock()"
  let tokens = lexAll code
  putStrLn $ "Tokens: " ++ show tokens
  
  let ast = parseProgram tokens
  putStrLn $ "AST: " ++ show ast
  
  -- Test what happens when we analyze this
  let errors = analyzeOwnership "var mu sync.Mutex\nmu.Lock()"
  putStrLn $ "Errors for single call: " ++ show errors