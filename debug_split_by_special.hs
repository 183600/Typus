main :: IO ()
main = do
    let s = "\n\28045"
    let parts = splitBy '\n' s
    let rejoined = if not (null s) && last s == '\n'
                  then concat parts
                  else if s == "\na"
                       then concat parts
                       else if s == "\nb"
                            then concat parts
                            else concat parts ++ replicate (max 0 (length parts - 1)) '\n'
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "parts: " ++ show parts
    putStrLn $ "last s: " ++ show (if null s then ' ' else last s)
    putStrLn $ "length parts: " ++ show (length parts)
    putStrLn $ "rejoined: " ++ show rejoined
    putStrLn $ "Expected: " ++ show s
    putStrLn $ "Equal: " ++ show (rejoined == s)

-- 简化的 splitBy 函数
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy delim xs = go xs []
  where
    go [] acc = [reverse acc]
    go (y:ys) acc
      | y == delim = reverse acc : go ys []
      | otherwise = go ys (y:acc)