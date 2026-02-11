import Utils

-- Test the exact failing case
main :: IO ()
main = do
  putStrLn "=== Testing exact failing case ==="
  -- The test says ["",""] but with mixed indentation it's actually ["\t  ", "\t  "]
  let testInput = ["\t  ", "\t  "]
  let input = unlines testInput
  putStrLn $ "Input lines: " ++ show testInput
  putStrLn $ "Input string: " ++ show input
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))
  putStrLn ""
