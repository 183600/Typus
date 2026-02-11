import Utils
import Data.Char (isPrint, isSpace)

-- 测试 prop_normalize_indentation_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with mixed..."
    
    -- 测试失败的情况：s = "\v"
    let s = "\v"
    let mixed = "\t  \t  " ++ s ++ "  \t  "
    let normalized = normalizeIndentation mixed
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Mixed: " ++ show mixed
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show mixed
    putStrLn $ "All isSpace mixed: " ++ show (all isSpace mixed)
    putStrLn $ "Any not isPrint s: " ++ show (any (not . isPrint) s)
    putStrLn $ "Test passes: " ++ show (normalized == mixed)