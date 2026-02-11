import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "a"
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "s: " ++ show s
    putStrLn $ "escaped: " ++ show escaped
    putStrLn $ "isCompleteStringLiteral escaped: " ++ show (isCompleteStringLiteral escaped)