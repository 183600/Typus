import Utils

main :: IO ()
main = do
    let s = ""
    let quoted = "\"" ++ s ++ "\""
    let incomplete = "\"" ++ s
    putStrLn $ "s = " ++ show s
    putStrLn $ "quoted = " ++ show quoted
    putStrLn $ "incomplete = " ++ show incomplete
    putStrLn $ "Utils.isCompleteStringLiteral quoted = " ++ show (Utils.isCompleteStringLiteral quoted)
    putStrLn $ "Utils.isCompleteStringLiteral incomplete = " ++ show (Utils.isCompleteStringLiteral incomplete)