import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "\"a"
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "quoted = " ++ show quoted
    putStrLn $ "incomplete = " ++ show incomplete
    putStrLn $ "isCompleteStringLiteral quoted = " ++ show (isCompleteStringLiteral quoted)
    putStrLn $ "isCompleteStringLiteral incomplete = " ++ show (isCompleteStringLiteral incomplete)
    putStrLn $ "Test expects: quoted=True, incomplete=False"
    putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral quoted && not (isCompleteStringLiteral incomplete))