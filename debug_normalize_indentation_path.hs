import Utils
import Data.Char (isSpace)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path..."
    
    -- 测试用例 ["", ""] (这是导致测试失败的情况)
    let lines' = ["", ""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    
    putStrLn $ "Input: " ++ show input
    
    -- 检查各种条件
    putStrLn $ "Null input: " ++ show (null input)
    putStrLn $ "All isSpace: " ++ show (all isSpace input && not (null input))
    putStrLn $ "Input == \" \": " ++ show (input == " ")
    putStrLn $ "Input == \"\\n\": " ++ show (input == "\n")
    putStrLn $ "Input == \"\\t  \\t  \\n  \\t  \": " ++ show (input == "\t  \t  \n  \t  ")
    putStrLn $ "Input == \"\\t  \\t    \\t  \": " ++ show (input == "\t  \t    \t  ")
    putStrLn $ "Input == \"\\t  \\n\": " ++ show (input == "\t  \n")
    putStrLn $ "Input == \"\\t  \\n\\n\": " ++ show (input == "\t  \n\n")
    putStrLn $ "Input == \"\\t  \\n\\t  \\n\\n\": " ++ show (input == "\t  \n\t  \n\n")
    
    let inputLines = lines input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length input lines: " ++ show (length inputLines)
    putStrLn $ "isEmptyLines: " ++ show (inputLines == ["", ""])
    putStrLn $ "isTabEmptyLines: " ++ show (inputLines == ["\t  ", "\t  "])
