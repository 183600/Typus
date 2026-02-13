import qualified Utils as U

main :: IO ()
main = do
  -- Test case for normalize indentation empty lines with "" + "\n\n"
  let s = ""
      withEmpty = s ++ "\n\n"
      normalized = U.normalizeIndentation withEmpty
  putStrLn $ "s: " ++ show s
  putStrLn $ "withEmpty: " ++ show withEmpty
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passes: " ++ show (normalized == "    ")