import qualified Utils as U

main :: IO ()
main = do
  -- Test case 1: Empty string
  let input1 = ""
      result1 = U.normalizeIndentation input1
  putStrLn $ "Test 1: input=" ++ show input1 ++ ", result=" ++ show result1
  
  -- Test case 2: Form feed
  let input2 = "\f"
      result2 = U.normalizeIndentation input2
  putStrLn $ "Test 2: input=" ++ show input2 ++ ", result=" ++ show result2
  
  -- Test case 3: Tab and space
  let input3 = "c\t"
      result3 = U.normalizeIndentation input3
  putStrLn $ "Test 3: input=" ++ show input3 ++ ", result=" ++ show result3
  
  -- Test case 4: Carriage return
  let input4 = "\r"
      result4 = U.normalizeIndentation input4
  putStrLn $ "Test 4: input=" ++ show input4 ++ ", result=" ++ show result4
  
  -- Test case 5: Multiline mixed
  let input5 = "\t  \n\t  \n"
      result5 = U.normalizeIndentation input5
  putStrLn $ "Test 5: input=" ++ show input5 ++ ", result=" ++ show result5
  
  -- Test case 6: Single tab
  let input6 = "\t"
      result6 = U.normalizeIndentation input6
  putStrLn $ "Test 6: input=" ++ show input6 ++ ", result=" ++ show result6