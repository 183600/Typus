import Utils

-- Test the new failing case
main :: IO ()
main = do
  let testInput = ["a", ""]
  let input = unlines testInput
  putStrLn $ "Input lines: " ++ show testInput
  putStrLn $ "Input string: " ++ show input
  let result = removeLineComments input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))