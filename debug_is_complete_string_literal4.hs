import Utils

main :: IO ()
main = do
    let s = "\\"  -- 输入是反斜杠
    let quoted = "\"" ++ s ++ "\""  -- "\"\\\""
    let incomplete = "\"" ++ s  -- "\"\\"
    putStrLn $ "s: " ++ show s
    putStrLn $ "quoted: " ++ show quoted
    putStrLn $ "incomplete: " ++ show incomplete
    putStrLn $ "isCompleteStringLiteral quoted: " ++ show (isCompleteStringLiteral quoted)
    putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (isCompleteStringLiteral incomplete)
    
    -- 检查每个的长度
    putStrLn $ "Length of quoted: " ++ show (length quoted)
    putStrLn $ "Length of incomplete: " ++ show (length incomplete)