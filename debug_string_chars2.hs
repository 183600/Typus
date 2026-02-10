import Data.Char

main :: IO ()
main = do
    let s = "\"" ++ "\\"  -- 双引号 + 反斜杠
    putStrLn $ "s: " ++ show s
    putStrLn $ "As list: " ++ show s
    putStrLn $ "Pattern represents: " ++ show "\"\\"
    putStrLn $ "s == pattern: " ++ show (s == "\"\\")
    
    -- 逐个字符比较
    putStrLn $ "First char: " ++ show (if not (null s) then head s else ' ')
    putStrLn $ "Second char: " ++ show (if length s >= 2 then s !! 1 else ' ')
    
    -- 检查模式
    case s of
      ['"','\\'] -> putStrLn "Matches pattern 1"
      ['"','\\'] -> putStrLn "Matches pattern 2"
      _ -> putStrLn "No pattern match"