import Utils

main :: IO ()
main = do
    let str = "\"b\""
    putStrLn $ "String: " ++ show str
    putStrLn $ "Length: " ++ show (length str)
    putStrLn $ "Last char: " ++ show (last str)
    putStrLn $ "Init: " ++ show (init str)
    putStrLn $ "Last of init: " ++ show (last (init str))
    putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral str)