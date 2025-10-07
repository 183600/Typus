import Ownership (analyzeOwnership, analyzeOwnershipDebug)

main :: IO ()
main = do
  -- Test with a simple identifier that should be parsed correctly
  let testCode1 = unlines ["x := 1", "y := x", "z := x"]
  putStrLn $ "Testing simple code: " ++ testCode1
  let (result1, debug1) = analyzeOwnershipDebug True testCode1
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "---"
  
  -- Test with the problematic slice literal
  let testCode2 = unlines ["x := []int{1}", "y := x", "z := x"]
  putStrLn $ "Testing slice literal code: " ++ testCode2
  let (result2, debug2) = analyzeOwnershipDebug True testCode2
  putStrLn $ "Result: " ++ show result2