import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    -- Test the failing case
    let str = ""
        comment = "a*"
        stringWithComment = "\"" ++ str ++ " /* " ++ comment ++ " */\""
        result = removeComments stringWithComment
        commentStr = "/* " ++ comment ++ " */"
        quotedStr = "\"" ++ str ++ "\""
    
    putStrLn $ "str: " ++ show str
    putStrLn $ "comment: " ++ show comment
    putStrLn $ "stringWithComment: " ++ show stringWithComment
    putStrLn $ "result: " ++ show result
    putStrLn $ "commentStr: " ++ show commentStr
    putStrLn $ "quotedStr: " ++ show quotedStr
    putStrLn $ "commentStr in result: " ++ show (commentStr `isInfixOf` result)
    putStrLn $ "quotedStr in result: " ++ show (quotedStr `isInfixOf` result)
    
    -- Test what the test expects
    putStrLn $ "\nTest expects:"
    putStrLn $ "1. not (commentStr `isInfixOf` result): " ++ show (not (commentStr `isInfixOf` result))
    putStrLn $ "2. quotedStr `isInfixOf` result: " ++ show (quotedStr `isInfixOf` result)
    putStrLn $ "Test passes: " ++ show (not (commentStr `isInfixOf` result) && quotedStr `isInfixOf` result)