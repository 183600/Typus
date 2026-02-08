import Utils
import Data.List (isInfixOf, isPrefixOf, all)
import Data.Char (isSpace)

main :: IO ()
main = do
    let str = "a"
        comment = ""
        stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
        result = removeComments stringWithComment
        commentStr = "/* " ++ comment ++ " */"
        quotedStr = "\"" ++ str ++ "\""
    
    putStrLn $ "Input string: " ++ str
    putStrLn $ "Comment: " ++ comment
    putStrLn $ "String with comment: " ++ stringWithComment
    putStrLn $ "Result: " ++ result
    putStrLn $ "Comment string: " ++ commentStr
    putStrLn $ "Quoted string: " ++ quotedStr
    putStrLn $ "Comment in result: " ++ show (commentStr `isInfixOf` result)
    putStrLn $ "Quoted string in result: " ++ show (quotedStr `isInfixOf` result)
    
    -- Test the fixed logic
    putStrLn "\n--- Testing fixed logic ---"
    let (beforeComment, fromComment) = breakOn "/*" stringWithComment
        afterComment = drop 2 fromComment
        afterBlock = if "*/" `isPrefixOf` afterComment
                     then drop 2 afterComment
                     else afterComment
        afterBlock2 = if "*/" `isPrefixOf` afterBlock
                      then drop 2 afterBlock
                      else afterBlock
        finalAfterBlock = if all isSpace afterBlock2 then "" else afterBlock2
        fixedResult = if null finalAfterBlock
                      then beforeComment ++ "\""
                      else beforeComment ++ finalAfterBlock
    
    putStrLn $ "beforeComment: " ++ beforeComment
    putStrLn $ "afterBlock2: " ++ show afterBlock2
    putStrLn $ "all isSpace afterBlock2: " ++ show (all isSpace afterBlock2)
    putStrLn $ "finalAfterBlock: " ++ show finalAfterBlock
    putStrLn $ "fixedResult: " ++ fixedResult
    putStrLn $ "quotedStr in fixedResult: " ++ show (quotedStr `isInfixOf` fixedResult)