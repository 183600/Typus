import Utils

main :: IO ()
main = do
  -- Test with a simple multi-line input
  let input1 = "    a\n    b\n    c\n"
  let result1 = Utils.normalizeIndentation input1
  putStrLn $ "Test 1 - Simple case:"
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "Output: " ++ show result1
  putStrLn $ ""
  
  -- Test with the actual code block
  let input2 = "    if condition {\n        // do something\n        return \n    }\n"
  let result2 = Utils.normalizeIndentation input2
  putStrLn $ "Test 2 - Code block:"
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Output: " ++ show result2