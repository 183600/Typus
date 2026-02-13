import Utils

-- 调试 splitBy 的行为
debugSplitBy :: Char -> String -> IO ()
debugSplitBy delim str = do
    putStrLn $ "=== debugSplitBy '" ++ [delim] ++ "' " ++ show str ++ " ==="
    let (part, rest) = break (== delim) str
    putStrLn $ "break: part=" ++ show part ++ ", rest=" ++ show rest
    case rest of
      [] -> putStrLn $ "rest empty, result: [" ++ part ++ "]"
      _:xs -> do
        putStrLn $ "rest not empty, xs=" ++ show xs ++ ", null part=" ++ show (null part)
        if null part
          then do
            putStrLn $ "null part is True, recursing with: " ++ show xs
            let result = splitBy delim xs
            putStrLn $ "recursive result: " ++ show result
            putStrLn $ "final result: " ++ show ("" : result)
          else do
            putStrLn $ "null part is False, using normal logic"

main :: IO ()
main = debugSplitBy '\n' "\na"