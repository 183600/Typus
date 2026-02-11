import Data.Char (isSpace)

-- 模拟hasClosingQuote函数
hasClosingQuote :: Char -> String -> Bool
hasClosingQuote _ [] = False
hasClosingQuote quote str' = go str' 0
  where
    go :: String -> Int -> Bool
    go [] _ = False
    go (x:xs) backslashCount = 
      if x == quote 
        then
             if odd backslashCount
               then go xs 0
               else case xs of
                      [] -> True
                      _ -> if all isSpace xs
                           then True
                           else False
        else if x == '\\'
             then go xs (backslashCount + 1)
             else go xs 0

main :: IO ()
main = do
    let input = "\"a\\\\\""
    let result = hasClosingQuote '"' (tail input)
    
    putStrLn $ "input = " ++ show input
    putStrLn $ "tail input = " ++ show (tail input)
    putStrLn $ "hasClosingQuote result = " ++ show result