main :: IO ()
main = do
    let str1 = "\\"  -- 反斜杠
    let str2 = "\\"  -- 反斜杠
    putStrLn $ "str1: " ++ show str1 ++ " (length " ++ show (length str1) ++ ")"
    putStrLn $ "str2: " ++ show str2 ++ " (length " ++ show (length str2) ++ ")"
    putStrLn $ "str1 == str2: " ++ show (str1 == str2)