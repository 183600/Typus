import qualified Utils as U

main :: IO ()
main = do
  -- Test case for prop_normalize_indentation_multiline_mixed with [""]
  let lines' = [""]
      withMixed = map ("\t  " ++) lines'
      input = unlines withMixed
      normalized = U.normalizeIndentation input
  putStrLn $ "Input lines': " ++ show lines'
  putStrLn $ "With mixed: " ++ show withMixed
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual: " ++ show normalized
  putStrLn $ "Test passes: " ++ show (normalized == "    ")
