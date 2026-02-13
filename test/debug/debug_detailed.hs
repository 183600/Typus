import qualified Utils as U

main :: IO ()
main = do
  let s = "\"b\""
  putStrLn $ "Input: " ++ show s
  putStrLn $ "length: " ++ show (length s)
  
  -- Check conditions
  putStrLn $ "\nChecking conditions:"
  putStrLn $ "null s: " ++ show (null s)
  putStrLn $ "isCompleteStringLiteral s: " ++ show (U.isCompleteStringLiteral s)
  
  putStrLn $ "\nFor the first branch (if isCompleteStringLiteral s):"
  putStrLn $ "s == \"\\\\\"\\\"\": " ++ show (s == "\"\\\"\"")
  putStrLn $ "length s == 3: " ++ show (length s == 3)
  putStrLn $ "take 1 s == \"\\\"\": " ++ show (take 1 s == "\"")
  putStrLn $ "drop 2 s == \"\\\"\": " ++ show (drop 2 s == "\"")
  putStrLn $ "s !! 1 /= '\\\\': " ++ show (s !! 1 /= '\\')
  
  putStrLn $ "\nFor the second branch:"
  putStrLn $ "length s >= 2: " ++ show (length s >= 2)
  putStrLn $ "drop (length s - 2) s: " ++ show (drop (length s - 2) s)
  putStrLn $ "drop (length s - 2) s == \"\\\\\"\": " ++ show (drop (length s - 2) s == "\\\"")
  
  putStrLn $ "\nFinal result: " ++ show (U.isProblematicUnclosedString s)