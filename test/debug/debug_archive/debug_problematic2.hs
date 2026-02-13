import Utils

main :: IO ()
main = do
  -- Test isProblematicUnclosedString with specific test cases
  let s = ""
  let closed = "\"" ++ s ++ "\""
  let unclosed = "\"" ++ s
  putStrLn $ "s = \"\""
  putStrLn $ "closed = " ++ show closed
  putStrLn $ "unclosed = " ++ show unclosed
  putStrLn $ "isProblematicUnclosedString closed = " ++ show (isProblematicUnclosedString closed)
  putStrLn $ "isProblematicUnclosedString unclosed = " ++ show (isProblematicUnclosedString unclosed)
  
  let s2 = "\""
  let closed2 = "\"" ++ s2 ++ "\""
  let unclosed2 = "\"" ++ s2
  putStrLn $ "\ns2 = \"\\\"\""
  putStrLn $ "closed2 = " ++ show closed2
  putStrLn $ "unclosed2 = " ++ show unclosed2
  putStrLn $ "isProblematicUnclosedString closed2 = " ++ show (isProblematicUnclosedString closed2)
  putStrLn $ "isProblematicUnclosedString unclosed2 = " ++ show (isProblematicUnclosedString unclosed2)