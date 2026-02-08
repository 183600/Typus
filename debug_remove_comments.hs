import Utils
import Data.List (isInfixOf)

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