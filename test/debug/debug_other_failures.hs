import qualified Utils as U

main :: IO ()
main = do
  -- Test case for normalize indentation empty lines with ""
  let input1 = ""
      result1 = U.normalizeIndentation input1
  putStrLn $ "Test 1: input=" ++ show input1 ++ ", result=" ++ show result1
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (result1 == "    ")
  
  -- Test case for normalizeIndentation code block with ""
  let input2 = ""
      result2 = U.normalizeIndentation input2
  putStrLn $ "\nTest 2: input=" ++ show input2 ++ ", result=" ++ show result2
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (result2 == "    ")
  
  -- Test case for normalizeIndentation nested with ""
  let input3 = ""
      result3 = U.normalizeIndentation input3
  putStrLn $ "\nTest 3: input=" ++ show input3 ++ ", result=" ++ show result3
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (result3 == "    ")
  
  -- Test case for minimal char property with '\ETX'
  let input4 = "\ETX"
      result4 = U.normalizeIndentation input4
  putStrLn $ "\nTest 4: input=" ++ show input4 ++ ", result=" ++ show result4
  putStrLn $ "Expected: " ++ show input4
  putStrLn $ "Test passes: " ++ show (result4 == input4)