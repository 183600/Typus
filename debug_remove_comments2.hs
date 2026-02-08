import Utils
import Data.List (isInfixOf, isPrefixOf)

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
    
    -- Debug the internal processing
    putStrLn "\n--- Debug ---"
    putStrLn $ "isCompleteStringLiteral s: " ++ show (isCompleteStringLiteral stringWithComment)
    putStrLn $ "isInfixOf \"/*\" s: " ++ show ("/*" `isInfixOf` stringWithComment)
    putStrLn $ "isInfixOf \"*/\" s: " ++ show ("*/" `isInfixOf` stringWithComment)
    
    let (beforeComment, fromComment) = breakOn "/*" stringWithComment
    putStrLn $ "beforeComment: " ++ beforeComment
    putStrLn $ "fromComment: " ++ fromComment
    
    let afterComment = drop 2 fromComment
    putStrLn $ "afterComment: " ++ afterComment
    
    let afterBlock = if "*/" `isPrefixOf` afterComment
                     then drop 2 afterComment
                     else afterComment
    putStrLn $ "afterBlock: " ++ afterBlock
    
    let afterBlock2 = if "*/" `isPrefixOf` afterBlock
                      then drop 2 afterBlock
                      else afterBlock
    putStrLn $ "afterBlock2: " ++ afterBlock2
    putStrLn $ "null afterBlock2: " ++ show (null afterBlock2)