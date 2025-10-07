import Ownership

main :: IO ()
main = do
  -- Test with multiple method calls
  let code = unlines ["var mu sync.Mutex", "mu.Lock()", "mu.Unlock()"]
  putStrLn $ "Testing multiple calls: " ++ show code
  let errors = analyzeOwnership code
  putStrLn $ "Errors: " ++ show errors
  
  -- Test with debug
  let (debugErrors, debugLog) = analyzeOwnershipDebug True code
  putStrLn $ "Debug errors: " ++ show debugErrors
  putStrLn "Debug log:"
  mapM_ putStrLn debugLog