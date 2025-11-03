import Ownership (analyzeOwnership, analyzeOwnershipDebug)

main :: IO ()
main = do
  let testCode = unlines ["x := []int{1}", "y := x", "z := x"]
  putStrLn $ "Testing code: " ++ testCode
  let (result, debug) = analyzeOwnershipDebug True testCode
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Debug output:"
  mapM_ putStrLn debug
  putStrLn $ "Expected: non-empty list (should detect double move)"
  putStrLn $ "Actual: " ++ show (length result) ++ " errors"