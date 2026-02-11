main :: IO ()
main = do
    let s = "\n\28045"
    putStrLn $ "s: " ++ show s
    
    -- 测试实际的 splitBy 函数行为
    let test1 = splitBy '\n' "\n\28045"
    putStrLn $ "splitBy '\\n' \"\\n\\28045\": " ++ show test1
    
    let test2 = splitBy '\n' "a\nb"
    putStrLn $ "splitBy '\\n' \"a\\nb\": " ++ show test2

-- 实际的 splitBy 函数实现
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delim str = 
  let (part, rest) = Prelude.break (== delim) str
  in case rest of
       [] -> [part]  -- No delimiter found, return the whole string
       [_] -> if delim == '\n' 
              then [part ++ "\n"]  -- Special case for newline: preserve the newline
              else part : [""]  -- Single delimiter at end
       _:xs -> if delim == '\n'
               then (part ++ "\n") : splitBy delim xs  -- Special case for newline: preserve the newline
               else part : splitBy delim xs  -- Continue with the rest