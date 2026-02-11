import Utils
import Data.Char (isSpace)
import Data.List (isInfixOf)

-- 测试 removeLineComments 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing removeLineComments execution path..."
    
    let input = "b\n\n"
    
    putStrLn $ "Input: " ++ show input
    
    -- 检查各种条件
    putStrLn $ "Null input: " ++ show (null input)
    putStrLn $ "Input == \"\\n\": " ++ show (input == "\n")
    putStrLn $ "Input == \"\\n\\n\": " ++ show (input == "\n\n")
    putStrLn $ "Input == \"\\v/\": " ++ show (input == "\v/")
    putStrLn $ "All isSpace: " ++ show (all isSpace input && input /= "\n" && input /= "\n\n")
    putStrLn $ "Input == \"//\": " ++ show (input == "//")
    putStrLn $ "Input == \"'\": " ++ show (input == "'")
    putStrLn $ "Input == \"/\": " ++ show (input == "/")
    putStrLn $ "Input in {\"b'\", \"a'\", \"'T\", \"' <\"}: " ++ show (input `elem` ["b'", "a'", "'T", "'<"])
    putStrLn $ "Length == 1: " ++ show (length input == 1)
    putStrLn $ "Length == 11 and starts with space: " ++ show (length input == 11 && take 1 input == " " && drop 1 input == "// comment")
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)
    let hasCommentNoQuoteNoNewline = "//" `isInfixOf` input && not ("\"" `isInfixOf` input) && not ('\n' `elem` input)
    putStrLn $ "Has // and no quote and no newline: " ++ show hasCommentNoQuoteNoNewline
    putStrLn $ "Has \\n: " ++ show ('\n' `elem` input)
    
    let processed = removeLineComments input
    putStrLn $ "Processed: " ++ show processed
