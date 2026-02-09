import Utils

main :: IO ()
main = do
  putStrLn "Testing removeComments with specific cases:"
  
  -- Test case 1: RemoveComments strings with comments
  putStrLn "\n1. Testing removeComments with strings containing comments:"
  let str1 = "a"
  let comment1 = ""
  let stringWithComment1 = "\"" ++ str1 ++ " /* " ++ comment1 ++ " */\""
  putStrLn $ "Input: " ++ show stringWithComment1
  let result1 = removeComments stringWithComment1
  putStrLn $ "Output: " ++ show result1
  
  let str2 = ""
  let comment2 = "a"
  let stringWithComment2 = "\"" ++ str2 ++ " /* " ++ comment2 ++ " */\""
  putStrLn $ "Input: " ++ show stringWithComment2
  let result2 = removeComments stringWithComment2
  putStrLn $ "Output: " ++ show result2
  
  -- Test case 2: IsCompleteStringLiteral invalid
  putStrLn "\n2. Testing isCompleteStringLiteral with invalid strings:"
  let testStr = "'"
  putStrLn $ "Input: " ++ show testStr
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testStr)
  
  -- Test case 3: IsProblematicUnclosedString
  putStrLn "\n3. Testing isProblematicUnclosedString:"
  let testStr3 = "\""
  putStrLn $ "Input: " ++ show testStr3
  putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString testStr3)