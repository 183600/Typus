import Utils

-- Test the normalizeIndentation function directly
main :: IO ()
main = do
  let input = "\t  \n\t  \n"
  putStrLn $ "Input: " ++ show input
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Output lines: " ++ show (lines result)
  putStrLn $ "Number of lines: " ++ show (length (lines result))