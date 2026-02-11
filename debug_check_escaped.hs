import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let str = "\"a\\\"\""
    putStrLn $ "str: " ++ show str
    putStrLn $ "last str: " ++ show (last str)
    putStrLn $ "str == \"\\\"a\\\"\\\": " ++ show (str == "\"a\"")
    putStrLn $ "last str == '\"': " ++ show (last str == '\"')
    putStrLn $ "isCompleteStringLiteral str: " ++ show (isCompleteStringLiteral str)