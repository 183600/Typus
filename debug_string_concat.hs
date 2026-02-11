import Utils

main :: IO ()
main = do
  putStrLn "=== Debugging string concatenation ==="
  
  let s = "\\"
  let part1 = "\""
  let part2 = s
  let part3 = "\\\\"
  let part4 = "\""
  let result = part1 ++ part2 ++ part3 ++ part4
  
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2  
  putStrLn $ "part3: " ++ show part3
  putStrLn $ "part4: " ++ show part4
  putStrLn $ "result: " ++ show result
  putStrLn $ "result chars: " ++ show (map (\c -> (c, fromEnum c)) result)
  putStrLn $ "length: " ++ show (length result)
  
  putStrLn "\n=== Testing isCompleteStringLiteral ==="
  putStrLn $ "isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral result)