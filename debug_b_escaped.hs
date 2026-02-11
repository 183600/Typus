import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "b"
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "s: " ++ show s
    putStrLn $ "escaped: " ++ show escaped
    putStrLn $ "isCompleteStringLiteral escaped: " ++ show (isCompleteStringLiteral escaped)