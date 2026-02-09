import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "Testing removeComments with specific cases:"
  
  -- Test case 1: RemoveComments strings with comments
  putStrLn "\n1. Testing removeComments with strings containing comments:"
  let str1 = ""
  let comment1 = "a"
  let stringWithComment1 = "\"" ++ str1 ++ " /* " ++ comment1 ++ " */\""
  putStrLn $ "Input: " ++ show stringWithComment1
  let result1 = removeComments stringWithComment1
  putStrLn $ "Output: " ++ show result1
  let commentStr1 = "/* " ++ comment1 ++ " */"
  let quotedStr1 = "\"" ++ str1 ++ "\""
  putStrLn $ "Comment removed: " ++ show (not (commentStr1 `isInfixOf` result1))
  putStrLn $ "String content preserved: " ++ show (quotedStr1 `isInfixOf` result1)
  
  -- Test case 2: RemoveComments strings with comments
  putStrLn "\n2. Testing removeComments with strings containing comments:"
  let str2 = "a"
  let comment2 = ""
  let stringWithComment2 = "\"" ++ str2 ++ " /* " ++ comment2 ++ " */\""
  putStrLn $ "Input: " ++ show stringWithComment2
  let result2 = removeComments stringWithComment2
  putStrLn $ "Output: " ++ show result2
  let commentStr2 = "/* " ++ comment2 ++ " */"
  let quotedStr2 = "\"" ++ str2 ++ "\""
  putStrLn $ "Comment removed: " ++ show (not (commentStr2 `isInfixOf` result2))
  putStrLn $ "String content preserved: " ++ show (quotedStr2 `isInfixOf` result2)