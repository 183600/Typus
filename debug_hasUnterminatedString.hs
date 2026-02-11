import Utils (removeLineComments)

-- Test hasUnterminatedString function
hasUnterminatedString :: String -> Bool
hasUnterminatedString s = go 0 False s
  where
    go :: Int -> Bool -> String -> Bool
    go _ _ [] = False
    go count inString ('\\':c:cs) = go count inString (c:cs)  -- Skip escaped characters
    go count True ('"':cs) = go (count + 1) False cs  -- Close string
    go count False ('"':cs) = go (count + 1) True cs   -- Open string
    go count inString ('\n':cs) = if inString then True else go count False cs  -- Check for newline in string
    go count inString (_:cs) = go count inString cs

main :: IO ()
main = do
  putStrLn "=== Testing hasUnterminatedString ==="
  
  let test1 = "\"\n// not comment\""
  putStrLn $ "test1: " ++ show test1
  putStrLn $ "hasUnterminatedString test1: " ++ show (hasUnterminatedString test1)
  
  let result1 = removeLineComments test1
  putStrLn $ "removeLineComments test1: " ++ show result1
  
  let test2 = "\"hello\""
  putStrLn $ "\ntest2: " ++ show test2
  putStrLn $ "hasUnterminatedString test2: " ++ show (hasUnterminatedString test2)
  
  let result2 = removeLineComments test2
  putStrLn $ "removeLineComments test2: " ++ show result2
  
  let test3 = "\"hello\nworld\""
  putStrLn $ "\ntest3: " ++ show test3
  putStrLn $ "hasUnterminatedString test3: " ++ show (hasUnterminatedString test3)
  
  let result3 = removeLineComments test3
  putStrLn $ "removeLineComments test3: " ++ show result3