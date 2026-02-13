import Data.Char

main :: IO ()
main = do
    let incomplete = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "incomplete: " ++ show incomplete ++ " (length " ++ show (length incomplete) ++ ")"
    putStrLn $ "Chars: " ++ show (map (\c -> (c, ord c)) incomplete)
    
    -- 检查特殊情况
    case incomplete of
      "\"\\" -> putStrLn "Matches special case \"\\\\ -> False\""
      _ -> putStrLn "Does not match special case"
      
    -- 检查特殊情况的具体字符
    let special = "\"" ++ "\\"
    putStrLn $ "special: " ++ show special ++ " (length " ++ show (length special) ++ ")"
    putStrLn $ "Chars: " ++ show (map (\c -> (c, ord c)) special)
    putStrLn $ "incomplete == special: " ++ show (incomplete == special)