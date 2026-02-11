import Data.Char

main :: IO ()
main = do
    let incomplete = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "incomplete: " ++ show incomplete
    putStrLn $ "Length: " ++ show (length incomplete)
    putStrLn $ "Chars: " ++ show (map (\c -> (c, ord c)) incomplete)
    
    -- 检查特殊情况
    putStrLn $ "Matches [\"\\]: " ++ show (incomplete == "\"\\")
    putStrLn $ "Matches \"\\\\\": " ++ show (incomplete == "\"\\")
    
    -- 检查字符序列
    putStrLn $ "As char list: " ++ show (incomplete)
    putStrLn $ "Pattern [\"\\] represents: " ++ show ['"', '\\']
    putStrLn $ "Pattern \"\\\\\" represents: " ++ show "\"\\"