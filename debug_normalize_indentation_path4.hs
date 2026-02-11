import Utils
import Data.Char (isSpace)

-- 测试 normalizeIndentation 函数的执行路径
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation execution path..."
    
    let input = "\f"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Null input: " ++ show (null input)
    putStrLn $ "All isSpace: " ++ show (all isSpace input && not (null input))
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized