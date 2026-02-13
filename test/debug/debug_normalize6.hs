import Utils

main :: IO ()
main = do
  -- Test case: prop_normalize_indentation_multiline_mixed with lines' = [""]
  let lines' = [""]
      withMixed = map ("\t  " ++) lines'
      input2 = unlines withMixed
      result2 = normalizeIndentation input2
  putStrLn $ "Test - lines' = [\"\"]"
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual: " ++ show result2
  putStrLn $ "Match: " ++ show (result2 == "    ")
  putStrLn ""