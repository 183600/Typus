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
    putStrLn $ "Input == special pattern 1: " ++ show (input == "\t  \t  \n  \t  ")
    putStrLn $ "Input == special pattern 2: " ++ show (input == "\t  \t    \t  ")
    putStrLn $ "Input == special pattern 3: " ++ show (input == "\t  \n\t  \n\n")
    putStrLn $ "All isSpace: " ++ show (all isSpace input && not (null input))
    
    let inputLines = lines input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length input lines: " ++ show (length inputLines)
    putStrLn $ "Single line: " ++ show (length inputLines <= 1)
    
    -- 检查单行处理部分
    if length inputLines <= 1
      then putStrLn "Taking single line path"
      else putStrLn "Taking multi-line path"