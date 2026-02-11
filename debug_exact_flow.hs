import Data.List (lines, unlines)

-- Check the exact flow
main :: IO ()
main = do
  let lines' = ["a\n"]
  let code = unlines lines'
  let inputLines = lines code
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "code: " ++ show code
  putStrLn $ "inputLines: " ++ show inputLines