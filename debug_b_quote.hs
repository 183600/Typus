import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "b\""
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    putStrLn $ "s: " ++ show s
    putStrLn $ "quoted: " ++ show quoted
    putStrLn $ "incomplete: " ++ show incomplete
    putStrLn $ "isCompleteStringLiteral quoted: " ++ show (isCompleteStringLiteral quoted)
    putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (isCompleteStringLiteral incomplete)
    putStrLn $ "Expected: quoted=True, incomplete=False"