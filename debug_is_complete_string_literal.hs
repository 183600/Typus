import Utils

-- 测试 prop_is_complete_string_literal 失败的情况
main :: IO ()
main = do
    putStrLn "Testing isCompleteStringLiteral..."
    
    -- 测试失败的情况：s = "a"
    let s = "a"
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Quoted: " ++ show quoted
    putStrLn $ "Incomplete: " ++ show incomplete
    putStrLn $ "isCompleteStringLiteral quoted: " ++ show (isCompleteStringLiteral quoted)
    putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (isCompleteStringLiteral incomplete)
    putStrLn $ "Expected: quoted = True, incomplete = False"
    putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral quoted && not (isCompleteStringLiteral incomplete))