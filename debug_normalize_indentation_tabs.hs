import Utils
import Data.List (isPrefixOf)

-- 测试 prop_normalize_indentation_tabs 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with tabs..."
    
    -- 根据测试失败信息，输入是 "b "
    let s = "b "
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "With tabs: " ++ show withTabs
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Starts with \"\\t\\t\": " ++ show ("\t\t" `isPrefixOf` normalized)
    
    -- 测试期望：不以 "\t\t" 开头
    let testPasses = not ("\t\t" `isPrefixOf` normalized)
    putStrLn $ "Test passes: " ++ show testPasses