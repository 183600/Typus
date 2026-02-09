import Utils
import Test.QuickCheck

main :: IO ()
main = do
    -- 手动测试 prop_is_complete_string_literal_invalid 的逻辑
    let s = "'"
    let validS = take 50 s
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
    
    putStrLn $ "输入 s = " ++ show s
    putStrLn $ "validS = " ++ show validS
    putStrLn $ "stringWithoutEndQuote = " ++ show stringWithoutEndQuote
    putStrLn $ "stringWithoutEndQuoteSingle = " ++ show stringWithoutEndQuoteSingle
    
    putStrLn $ "\nnull validS = " ++ show (null validS)
    
    if null validS
    then do
        let test1 = isCompleteStringLiteral "\"\\"
        let test2 = isCompleteStringLiteral "'\\"
        putStrLn $ "测试 \"\\\\\" -> " ++ show test1
        putStrLn $ "测试 \"'\\\\\" -> " ++ show test2
        putStrLn $ "not test1 && not test2 = " ++ show (not test1 && not test2)
    else do
        let test1 = isCompleteStringLiteral stringWithoutEndQuote
        let test2 = isCompleteStringLiteral stringWithoutEndQuoteSingle
        putStrLn $ "测试 " ++ show stringWithoutEndQuote ++ " -> " ++ show test1
        putStrLn $ "测试 " ++ show stringWithoutEndQuoteSingle ++ " -> " ++ show test2
        putStrLn $ "not test1 && not test2 = " ++ show (not test1 && not test2)
    
    -- 直接测试 isCompleteStringLiteral "'"
    putStrLn $ "\n直接测试 isCompleteStringLiteral \"'\" = " ++ show (isCompleteStringLiteral "'")
    
    -- 使用 QuickCheck 生成一些测试数据
    putStrLn "\n使用 QuickCheck 生成测试数据:"
    quickCheck $ \s -> 
        let validS = take 50 s
            stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
        in if null validS
           then not (isCompleteStringLiteral "'\\")
           else not (isCompleteStringLiteral stringWithoutEndQuoteSingle)