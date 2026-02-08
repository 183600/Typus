import Utils
import Data.List (isInfixOf, isPrefixOf, all)
import Data.Char (isSpace)

-- Copy the fixed logic directly here to test
removeCommentsFixed :: String -> String
removeCommentsFixed s = 
  if isCompleteStringLiteral s && "/*" `isInfixOf` s && "*/" `isInfixOf` s
    then let (beforeComment, fromComment) = breakOn "/*" s
             afterComment = drop 2 fromComment  -- Drop "/*"
             afterBlock = if "*/" `isPrefixOf` afterComment
                          then drop 2 afterComment  -- Drop "*/"
                          else afterComment
             afterBlock2 = if "*/" `isPrefixOf` afterBlock
                           then drop 2 afterBlock  -- Drop "*/"
                           else afterBlock
             -- Trim trailing whitespace before comment for cleaner output
             trimmedBefore = reverse $ dropWhile isSpace $ reverse beforeComment
             -- Trim trailing whitespace after comment removal for empty comments
             finalAfterBlock = if all isSpace afterBlock2 then "" else afterBlock2
         in if null finalAfterBlock
            then trimmedBefore ++ "\""  -- Add closing quote
            else trimmedBefore ++ finalAfterBlock
    else s  -- Simplified for testing

main :: IO ()
main = do
    let s = "\"a /*  */\""
    
    putStrLn $ "Original string: " ++ show s
    
    -- Test the fixed logic
    let result = removeCommentsFixed s
    putStrLn $ "Fixed result: " ++ show result
    
    -- Test the actual function
    let actualResult = removeComments s
    putStrLn $ "Actual result: " ++ show actualResult
    
    -- Check if they match
    putStrLn $ "Results match: " ++ show (result == actualResult)