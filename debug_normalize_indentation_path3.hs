import Utils
import Data.Char (isSpace)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path..."
    
    let input = "\t  \n"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input == \"\\t  \\n\": " ++ show (input == "\t  \n")
    putStrLn $ "Input == \"\\t  \\n\\t  \\n\\n\": " ++ show (input == "\t  \n\t  \n\n")
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized