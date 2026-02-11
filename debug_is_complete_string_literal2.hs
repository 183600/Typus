import Utils

main :: IO ()
main = do
    let s = "\\"
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    putStrLn $ "s = " ++ show s
    putStrLn $ "quoted = " ++ show quoted
    putStrLn $ "incomplete = " ++ show incomplete
    putStrLn $ "isCompleteStringLiteral quoted = " ++ show (Utils.isCompleteStringLiteral quoted)
    putStrLn $ "isCompleteStringLiteral incomplete = " ++ show (Utils.isCompleteStringLiteral incomplete)