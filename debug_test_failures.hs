import Utils (normalizeIndentation)
import Data.List (intercalate)

-- 测试失败的用例
main :: IO ()
main = do
    putStrLn "=== Testing normalizeIndentation ==="
    
    -- 测试用例1: "\n"
    let test1 = "\n"
    let result1 = normalizeIndentation test1
    putStrLn $ "Input: " ++ show test1
    putStrLn $ "Result: " ++ show result1
    putStrLn $ "Expected: \"    \" (4 spaces)"
    
    -- 测试用例2: "A\n" 的 lines 行为
    putStrLn "\n=== Testing lines behavior ==="
    let test2 = "A\n"
    let lines2 = lines test2
    let rejoined = intercalate "\n" lines2
    putStrLn $ "Input: " ++ show test2
    putStrLn $ "lines result: " ++ show lines2
    putStrLn $ "rejoined: " ++ show rejoined
    putStrLn $ "Expected: \"A\" (lines removes trailing newline)"