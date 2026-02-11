import Utils

-- Test the ["\n"] case
main :: IO ()
main = do
  let testInput = ["\n"]
  let input = unlines testInput
  putStrLn $ "Input lines: " ++ show testInput
  putStrLn $ "Input string: " ++ show input
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))