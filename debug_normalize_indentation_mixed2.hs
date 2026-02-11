import Utils
import Data.Char (isPrint, isSpace)

-- 测试 prop_normalize_indentation_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with mixed..."
    
    -- 测试一个具体的情况
    let s = "test"  -- 非空字符串
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Mixed: " ++ show mixed
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "All isSpace mixed: " ++ show (all isSpace mixed)
    putStrLn $ "Any not isPrint s: " ++ show (any (not . isPrint) s)
    putStrLn $ "Expected: " ++ show mixed
    putStrLn $ "Test passes: " ++ show (normalized == mixed)
    
    -- 测试多行情况
    putStrLn "\n--- Testing multiline ---"
    let lines' = ["test1", "test2"]
    let withMixed = map ("\t  " ++) lines'
    let normalizedMulti = normalizeIndentation (unlines withMixed)
    let normLines = lines normalizedMulti
    
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Normalized: " ++ show normalizedMulti
    putStrLn $ "Normalized lines: " ++ show normLines
    putStrLn $ "Expected length: " ++ show (length lines')
    putStrLn $ "Actual length: " ++ show (length normLines)
    putStrLn $ "Test passes: " ++ show (length normLines == length lines')