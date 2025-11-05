import Ownership (analyzeOwnership)

main :: IO ()
main = do
  let testCode = unlines ["x := []int{1}", "y := x", "z := x"]
  putStrLn $ "Testing code: " ++ testCode
  let result = analyzeOwnership testCode
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: non-empty list (should detect double move)"
  putStrLn $ "Actual: " ++ show (length result) ++ " errors"