import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let s = "a\\"
    let withBackslash = "\"" ++ s ++ "\\\\" ++ "\""
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "withBackslash = " ++ show withBackslash
    putStrLn $ "isCompleteStringLiteral withBackslash = " ++ show (isCompleteStringLiteral withBackslash)
    putStrLn $ "Test expects: True"
    putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral withBackslash)