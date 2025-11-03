import Ownership (analyzeOwnership, analyzeOwnershipDebug)

main :: IO ()
main = do
  -- Test step by step
  let step1 = "x := 1"
  putStrLn $ "Step 1: " ++ step1
  let (result1, debug1) = analyzeOwnershipDebug True step1
  putStrLn $ "Result: " ++ show result1
  putStrLn "---"
  
  let step2 = "x := 1\ny := x"
  putStrLn $ "Step 2: " ++ step2
  let (result2, debug2) = analyzeOwnershipDebug True step2
  putStrLn $ "Result: " ++ show result2
  putStrLn "---"
  
  let step3 = "x := 1\ny := x\nz := x"
  putStrLn $ "Step 3: " ++ step3
  let (result3, debug3) = analyzeOwnershipDebug True step3
  putStrLn $ "Result: " ++ show result3