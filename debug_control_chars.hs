import Utils

-- Test the new failing cases
main :: IO ()
main = do
  -- Test carriage return
  let test1 = "\r"
  let result1 = normalizeIndentation test1
  putStrLn $ "Input: " ++ show test1
  putStrLn $ "Output: " ++ show result1
  putStrLn ""
  
  -- Test form feed
  let test2 = "\f"
  let result2 = normalizeIndentation test2
  putStrLn $ "Input: " ++ show test2
  putStrLn $ "Output: " ++ show result2
  putStrLn ""