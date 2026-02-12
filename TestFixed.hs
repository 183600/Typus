import qualified Utils as U

main :: IO ()
main = do
  -- 测试a\t的情况
  let testInput1 = "a\t"
  let result1 = U.normalizeIndentation testInput1
  putStrLn $ "Test 1 - Input: " ++ show testInput1
  putStrLn $ "Test 1 - Result: " ++ show result1
  putStrLn $ "Test 1 - Expected: " ++ show "a "
  putStrLn $ "Test 1 - " ++ if result1 == "a " then "PASSED" else "FAILED"
  
  -- 测试\t\f的情况
  let testInput2 = "\t\f"
  let result2 = U.normalizeIndentation testInput2
  putStrLn $ "\nTest 2 - Input: " ++ show testInput2
  putStrLn $ "Test 2 - Result: " ++ show result2
  putStrLn $ "Test 2 - Expected: " ++ show "    "
  putStrLn $ "Test 2 - " ++ if result2 == "    " then "PASSED" else "FAILED"