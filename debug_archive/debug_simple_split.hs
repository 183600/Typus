-- 简单的 splitBy 实现
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delim str = 
  let (part, rest) = break (== delim) str
  in case rest of
       [] -> [part]  -- No delimiter found
       _:xs -> part : splitBy delim xs  -- Recursively process the rest

-- 测试
main :: IO ()
main = do
    putStrLn $ "splitBy '\\n' \"\\na\" = " ++ show (splitBy '\n' "\na")
    putStrLn $ "Expected: [\"\", \"a\"]"