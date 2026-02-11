import Utils
import Data.List (isInfixOf)

-- 测试 prop_normalize_indentation_empty_lines 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with empty lines..."
    
    -- 测试空字符串的情况
    let s = ""
    let withEmpty = s ++ "\n\n"
    let normalized = normalizeIndentation withEmpty
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "With empty: " ++ show withEmpty
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (normalized == "    ")
    
    -- 测试非空字符串的情况
    let s2 = "a"
    let withEmpty2 = s2 ++ "\n\n"
    let normalized2 = normalizeIndentation withEmpty2
    
    putStrLn $ "\nInput s: " ++ show s2
    putStrLn $ "With empty: " ++ show withEmpty2
    putStrLn $ "Normalized: " ++ show normalized2
    putStrLn $ "Contains \"\\n\\n\": " ++ show ("\n\n" `isInfixOf` normalized2)
    putStrLn $ "Test passes: " ++ show ("\n\n" `isInfixOf` normalized2)