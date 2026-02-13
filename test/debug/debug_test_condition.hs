main :: IO ()
main = do
  let s = "//5\n"
  putStrLn $ "s: " ++ show s
  putStrLn $ "Length: " ++ show (length s)
  putStrLn $ "Take 2: " ++ show (take 2 s)
  putStrLn $ "Last: " ++ show (last s)
  putStrLn $ "Condition: " ++ show (length s == 4 && take 2 s == "//" && last s == '\n')
  
  if length s == 4 && take 2 s == "//" && last s == '\n'
    then putStrLn $ "Result: " ++ show ([s !! 2] ++ "\n")
    else putStrLn "Condition not matched"