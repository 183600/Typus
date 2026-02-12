import Utils

main :: IO ()
main = do
  let input = "    if condition {\n        // do something\n        return \n    }\n"
  let result = Utils.normalizeIndentation input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  
  -- Check if it's a single line according to the function
  let inputLines = lines input
  putStrLn $ "Number of lines: " ++ show (length inputLines)
  putStrLn $ "Should go to multi-line section: " ++ show (length inputLines > 1)