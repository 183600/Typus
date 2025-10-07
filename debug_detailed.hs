import Ownership

main :: IO ()
main = do
  -- Test just the method calls
  let code1 = "mu.Lock()"
  putStrLn $ "Testing: " ++ code1
  let errors1 = analyzeOwnership code1
  putStrLn $ "Errors: " ++ show errors1
  
  let code2 = "mu.Unlock()"
  putStrLn $ "Testing: " ++ code2
  let errors2 = analyzeOwnership code2
  putStrLn $ "Errors: " ++ show errors2
  
  -- Test with variable declaration
  let code3 = unlines ["var mu sync.Mutex", "mu.Lock()"]
  putStrLn $ "Testing with declaration: " ++ show code3
  let errors3 = analyzeOwnership code3
  putStrLn $ "Errors: " ++ show errors3