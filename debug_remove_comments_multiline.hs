import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    -- Test the failing case
    let before = "'"
        after = ""
        codeWithComment = before ++ "/* " ++ "comment" ++ " */" ++ after
        withoutComments = removeComments codeWithComment
    
    putStrLn $ "before: " ++ show before
    putStrLn $ "after: " ++ show after
    putStrLn $ "codeWithComment: " ++ show codeWithComment
    putStrLn $ "withoutComments: " ++ show withoutComments
    putStrLn $ "has /* in result: " ++ show ("/*" `isInfixOf` withoutComments)
    putStrLn $ "has */ in result: " ++ show ("*/" `isInfixOf` withoutComments)
    
    -- Test the expected result
    let expected = before ++ after
    putStrLn $ "expected: " ++ show expected
    putStrLn $ "test passes: " ++ show (withoutComments == expected)