import qualified Utils as U

main :: IO ()
main = do
  -- Test case for prop_normalize_indentation_mixed with "\f"
  let s = "\f"
      mixed = "\t  \t  " ++ s ++ "  \t  "
      normalized = U.normalizeIndentation mixed
  putStrLn $ "s: " ++ show s
  putStrLn $ "mixed: " ++ show mixed
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (normalized == "    ")