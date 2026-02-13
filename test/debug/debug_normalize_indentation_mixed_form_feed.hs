import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with form feed..."
  
  -- Test with s = "\f"
  let testInput = "\f"
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  let normalized = U.normalizeIndentation mixed
  
  putStrLn $ "Input s: " ++ show testInput
  putStrLn $ "Mixed string: " ++ show mixed
  putStrLn $ "Normalized: " ++ show normalized
  
  -- According to the test, when s contains non-printable characters,
  -- the normalized result should be the original mixed string
  let expected = mixed
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Test passed: " ++ show (normalized == expected)