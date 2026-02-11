import Utils
import Data.List (isInfixOf)

-- 测试 prop_normalize_indentation_empty_lines 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with empty lines..."
    
    -- 根据测试失败信息，输入是 " " 或 "\v" 或 "\n"
    let testCases = [" ", "\v", "\n"]
    mapM_ (\s -> do
        let withEmpty = s ++ "\n\n"
        let normalized = normalizeIndentation withEmpty
        putStrLn $ "Input s: " ++ show s
        putStrLn $ "With empty: " ++ show withEmpty
        putStrLn $ "Normalized: " ++ show normalized
        putStrLn $ "Contains \"\\n\\n\": " ++ show ("\n\n" `isInfixOf` normalized)
        putStrLn $ "---"
        ) testCases