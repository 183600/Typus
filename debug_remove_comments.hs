import Utils

main :: IO ()
main = do
  let input = "//\""
  let result = removeComments input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show (length $ filter (== '"') input)
  putStrLn $ "Result quotes: " ++ show (length $ filter (== '"') result)