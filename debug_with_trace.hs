import Utils

-- Add debug logging
main :: IO ()
main = do
  putStrLn "=== Testing with debug ==="
  let testInput = ["\t  ", "\t  "]
  let input = unlines testInput
  putStrLn $ "Input lines: " ++ show testInput
  putStrLn $ "Input string: " ++ show input
  
  let inputLines = lines input
  putStrLn $ "After lines(): " ++ show inputLines
  
  let isEmptyLines = inputLines == ["", ""]
  let isTabEmptyLines = inputLines == ["\t  ", "\t  "]
  putStrLn $ "isEmptyLines: " ++ show isEmptyLines
  putStrLn $ "isTabEmptyLines: " ++ show isTabEmptyLines
  
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))