import Utils (removeComments)
import Data.List (isInfixOf)

main :: IO ()
main = do
    let testCase = "\"string // not comment\" // real comment"
        result = removeComments testCase
        hasLineComment = "//" `isInfixOf` result
        hasBlockComment = "/*" `isInfixOf` result
    
    putStrLn $ "Input: " ++ show testCase
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Has // in output: " ++ show hasLineComment
    putStrLn $ "Has /* in output: " ++ show hasBlockComment
    
    -- 测试简单的removeComments
    let simpleTest = "code // comment"
        simpleResult = removeComments simpleTest
    putStrLn $ "\nSimple test:"
    putStrLn $ "Input: " ++ show simpleTest
    putStrLn $ "Output: " ++ show simpleResult
    putStrLn $ "Has // in output: " ++ show ("//" `isInfixOf` simpleResult)