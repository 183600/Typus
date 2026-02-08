import Utils
import Data.List (isInfixOf, isPrefixOf, all)
import Data.Char (isSpace)

main :: IO ()
main = do
    let str = "a"
        comment = ""
        stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
    
    putStrLn $ "Original string: " ++ show stringWithComment
    
    -- Let's check each character
    putStrLn $ "Characters: " ++ show (zip [0..] stringWithComment)
    
    -- Check where "/*" appears
    putStrLn $ "Position of \"/*\": " ++ show (findSubstring "/*" stringWithComment)
    putStrLn $ "Position of \"*/\": " ++ show (findSubstring "*/" stringWithComment)
    
    -- Let's manually break the string
    let (beforeComment, fromComment) = breakOn "/*" stringWithComment
    putStrLn $ "beforeComment: " ++ show beforeComment
    putStrLn $ "fromComment: " ++ show fromComment
    
    let afterComment = drop 2 fromComment  -- Drop "/*"
    putStrLn $ "afterComment: " ++ show afterComment
    
    let afterBlock = if "*/" `isPrefixOf` afterComment
                     then drop 2 afterComment  -- Drop "*/"
                     else afterComment
    putStrLn $ "afterBlock: " ++ show afterBlock
    
    let afterBlock2 = if "*/" `isPrefixOf` afterBlock
                      then drop 2 afterBlock  -- Drop "*/"
                      else afterBlock
    putStrLn $ "afterBlock2: " ++ show afterBlock2
    
    -- Check if beforeComment ends with space
    putStrLn $ "beforeComment ends with space: " ++ show (if not (null beforeComment) then last beforeComment == ' ' else False)
    
    -- Check what the expected result should be
    let quotedStr = "\"" ++ str ++ "\""
    putStrLn $ "Expected result: " ++ show quotedStr
    
    -- Check the actual result
    let result = removeComments stringWithComment
    putStrLn $ "Actual result: " ++ show result

findSubstring :: String -> String -> Int
findSubstring pat str = go pat str 0
  where
    go _ [] _ = -1
    go p s@(c:cs) n
      | p `isPrefixOf` s = n
      | otherwise = go p cs (n + 1)