import Utils

main :: IO ()
main = do
  let input = "\n\n"
  let result = Utils.normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Matches: " ++ show (result == "    ")