import qualified Utils as U

main :: IO ()
main = do
  -- Test case for prop_normalize_indentation_relative with "\f"
  let input2 = "\f"
      result2 = U.normalizeIndentation input2
  putStrLn $ "Test 2: input=" ++ show input2 ++ ", result=" ++ show result2
  putStrLn $ "Expected result: " ++ show input2
  putStrLn $ "Test passes: " ++ show (result2 == input2)
  
  -- Test case for prop_normalize_indentation_relative with "c\t"
  let input3 = "c\t"
      result3 = U.normalizeIndentation input3
  putStrLn $ "\nTest 3: input=" ++ show input3 ++ ", result=" ++ show result3
  putStrLn $ "Expected result: " ++ show "c "
  putStrLn $ "Test passes: " ++ show (result3 == "c ")
  
  -- Test case for prop_normalize_indentation_multiline_mixed with [""]
  let lines' = [""]
      withMixed = map ("\t  " ++) lines'
      input5 = unlines withMixed
      result5 = U.normalizeIndentation input5
  putStrLn $ "\nTest 5: input lines': " ++ show lines'
  putStrLn $ "Input: " ++ show input5
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual: " ++ show result5
  putStrLn $ "Test passes: " ++ show (result5 == "    ")
  
  -- Test case for tab character
  let input6 = "\t"
      result6 = U.normalizeIndentation input6
  putStrLn $ "\nTest 6: input=" ++ show input6 ++ ", result=" ++ show result6
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (result6 == "    ")