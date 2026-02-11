-- 检查字符串是否以双反斜杠结尾
endsWithDoubleBackslash :: String -> Bool
endsWithDoubleBackslash [] = False
endsWithDoubleBackslash [_] = False
endsWithDoubleBackslash str = 
  let lastTwo = drop (length str - 2) str
  in lastTwo == "\\\\"

main :: IO ()
main = do
    putStrLn "=== Testing prop_is_complete_string_literal_escaped failure case ==="
    
    -- 失败案例: s = "a\""
    let s = "a\""
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "s: " ++ show s
    putStrLn $ "escaped: " ++ show escaped
    putStrLn $ "escaped length: " ++ show (length escaped)
    putStrLn $ "escaped chars: " ++ show (map (\c -> (c, fromEnum c)) escaped)
    putStrLn $ "endsWithDoubleBackslash escaped: " ++ show (endsWithDoubleBackslash escaped)
    putStrLn $ "Last two chars: " ++ show (drop (length escaped - 2) escaped)