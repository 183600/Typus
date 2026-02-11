import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "b\""
    let incomplete = "\"" ++ s
    putStrLn $ "s: " ++ show s
    putStrLn $ "incomplete: " ++ show incomplete
    putStrLn $ "last incomplete: " ++ show (last incomplete)
    putStrLn $ "incomplete ends with \"\": " ++ show (last incomplete == '"')
    putStrLn $ "incomplete ends with \\\\: " ++ show (last incomplete == '\\')
    putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (isCompleteStringLiteral incomplete)