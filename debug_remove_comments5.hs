import Utils
import Data.List (isInfixOf, isPrefixOf, all)
import Data.Char (isSpace)

main :: IO ()
main = do
    let str = "a"
        comment = ""
        stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
    
    putStrLn $ "Original string: " ++ show stringWithComment
    
    -- Test the fixed logic
    let (beforeComment, fromComment) = breakOn "/*" stringWithComment
        afterComment = drop 2 fromComment
        afterBlock = if "*/" `isPrefixOf` afterComment
                     then drop 2 afterComment
                     else afterComment
        afterBlock2 = if "*/" `isPrefixOf` afterBlock
                      then drop 2 afterBlock
                      else afterBlock
        -- Trim trailing whitespace before comment for cleaner output
        trimmedBefore = reverse $ dropWhile isSpace $ reverse beforeComment
        -- Trim trailing whitespace after comment removal for empty comments
        finalAfterBlock = if all isSpace afterBlock2 then "" else afterBlock2
        fixedResult = if null finalAfterBlock
                      then trimmedBefore ++ "\""
                      else trimmedBefore ++ finalAfterBlock
    
    putStrLn $ "beforeComment: " ++ show beforeComment
    putStrLn $ "trimmedBefore: " ++ show trimmedBefore
    putStrLn $ "afterBlock2: " ++ show afterBlock2
    putStrLn $ "finalAfterBlock: " ++ show finalAfterBlock
    putStrLn $ "fixedResult: " ++ show fixedResult
    
    -- Check the actual result
    let result = removeComments stringWithComment
    putStrLn $ "Actual result: " ++ show result
    
    -- Check if the test would pass
    let quotedStr = "\"" ++ str ++ "\""
    putStrLn $ "Test would pass: " ++ show (quotedStr `isInfixOf` result)