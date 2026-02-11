import Utils

-- Test the failing case
main :: IO ()
main = do
  let lines' = ["a\n"]
  let code = unlines lines'
  let processed = Utils.removeLineComments code
  let procLines = lines processed
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "code: " ++ show code
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "procLines: " ++ show procLines
  putStrLn $ "length procLines: " ++ show (length procLines)
