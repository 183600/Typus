import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with form feed and newline..."
  
  -- Test with s = "\f\n"
  let testInput = "\f\n"
  let normalized = U.normalizeIndentation testInput
  
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Input chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") testInput
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Normalized chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") normalized
  
  -- According to the test, "\f\n" should be converted to "    "
  let expected = "    "
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Test passed: " ++ show (normalized == expected)