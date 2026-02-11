import Utils

main :: IO ()
main = do
    let s1 = "\""
    let s2 = "\"\\"
    putStrLn $ "Testing isCompleteStringLiteral with s1: " ++ show s1 ++ " -> " ++ show (isCompleteStringLiteral s1)
    putStrLn $ "Testing isCompleteStringLiteral with s2: " ++ show s2 ++ " -> " ++ show (isCompleteStringLiteral s2)