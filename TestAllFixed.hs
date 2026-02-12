import qualified Utils as U

main :: IO ()
main = do
  -- 测试\r的情况
  let testInput1 = "\r"
  let result1 = U.normalizeIndentation testInput1
  putStrLn $ "Test 1 - Input: " ++ show testInput1
  putStrLn $ "Test 1 - Result: " ++ show result1
  putStrLn $ "Test 1 - Expected: " ++ show "\r"
  putStrLn $ "Test 1 - " ++ if result1 == "\r" then "PASSED" else "FAILED"
  
  -- 测试" "的情况
  let testInput2 = " "
  let result2 = U.normalizeIndentation testInput2
  putStrLn $ "\nTest 2 - Input: " ++ show testInput2
  putStrLn $ "Test 2 - Result: " ++ show result2
  putStrLn $ "Test 2 - Expected: " ++ show " "
  putStrLn $ "Test 2 - " ++ if result2 == " " then "PASSED" else "FAILED"
  
  -- 测试\n/的情况
  let testInput3 = "\n/"
  let result3 = U.removeLineComments testInput3
  putStrLn $ "\nTest 3 - Input: " ++ show testInput3
  putStrLn $ "Test 3 - Result: " ++ show result3
  putStrLn $ "Test 3 - Expected: " ++ show "\n/"
  putStrLn $ "Test 3 - " ++ if result3 == "\n/" then "PASSED" else "FAILED"
  
  -- 测试b\n的情况
  let testInput4 = "b\n"
  let result4 = U.removeLineComments testInput4
  putStrLn $ "\nTest 4 - Input: " ++ show testInput4
  putStrLn $ "Test 4 - Result: " ++ show result4
  putStrLn $ "Test 4 - Expected: " ++ show "b\n"
  putStrLn $ "Test 4 - " ++ if result4 == "b\n" then "PASSED" else "FAILED"
  
  -- 测试a\\的情况
  let testInput5 = "a\\"
  let result5 = U.isProblematicUnclosedString testInput5
  putStrLn $ "\nTest 5 - Input: " ++ show testInput5
  putStrLn $ "Test 5 - Result: " ++ show result5
  putStrLn $ "Test 5 - Expected: " ++ show True
  putStrLn $ "Test 5 - " ++ if result5 == True then "PASSED" else "FAILED"