import Utils

main :: IO ()
main = do
    let s = "a"
    let mixed = "  \t  " ++ s
    putStrLn $ "Input: " ++ show mixed
    putStrLn $ "Has tab: " ++ show ('\t' `elem` mixed)
    
    -- 测试转换函数
    let converted = map (\c -> if c == '\t' then ' ' else c) mixed
    putStrLn $ "After conversion: " ++ show converted
    putStrLn $ "Has tab after conversion: " ++ show ('\t' `elem` converted)
    
    -- 测试 normalizeIndentation
    let normalized = normalizeIndentation mixed
    putStrLn $ "After normalizeIndentation: " ++ show normalized
    putStrLn $ "Has tab after normalizeIndentation: " ++ show ('\t' `elem` normalized)