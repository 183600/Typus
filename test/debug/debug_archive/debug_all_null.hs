-- Test all null behavior
main :: IO ()
main = do
  let lines1 = ["",""]
  putStrLn $ "lines1: " ++ show lines1
  putStrLn $ "null \"\": " ++ show (null "")
  putStrLn $ "all null lines1: " ++ show (all null lines1)
  
  let lines2 = [""]
  putStrLn $ "\nlines2: " ++ show lines2
  putStrLn $ "all null lines2: " ++ show (all null lines2)
  
  let lines3 = ["a",""]
  putStrLn $ "\nlines3: " ++ show lines3
  putStrLn $ "null \"a\": " ++ show (null "a")
  putStrLn $ "all null lines3: " ++ show (all null lines3)