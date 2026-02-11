import Utils
import Data.Char (isSpace)

-- 测试 prop_normalize_indentation_relative 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation relative..."
    
    -- 测试失败的情况：s = "\f"
    let s = "\f"
    let normalized = normalizeIndentation s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (normalized == "    ")
    
    -- 检查是否是空白字符
    putStrLn $ "All isSpace: " ++ show (all isSpace s)