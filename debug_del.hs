import Utils

main :: IO ()
main = do
  let lines' = ["\DEL\n"]
  let code = unlines lines'
  let processed = Utils.removeLineComments code
  let procLines = lines processed
  putStrLn $ "Input lines: " ++ show lines'
  putStrLn $ "Code: " ++ show code
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed lines: " ++ show procLines
  putStrLn $ "Expected lines: " ++ show (length lines')
  putStrLn $ "Actual lines: " ++ show (length procLines)