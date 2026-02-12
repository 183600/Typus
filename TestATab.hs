import qualified Utils as U

main :: IO ()
main = do
  let testInput = "a\t"
  let normalized = U.normalizeIndentation testInput
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if normalized == "a " then "PASSED" else "FAILED"