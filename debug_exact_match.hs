main :: IO ()
main = do
  let input1 = "\t\ta\t"
  let input2 = "\t  \n"
  
  putStrLn $ "input1: " ++ show input1
  putStrLn $ "input1 == \"\\t\\ta\\t\": " ++ show (input1 == "\t\ta\t")
  putStrLn $ "length input1: " ++ show (length input1)
  putStrLn $ "map fromEnum input1: " ++ show (map (fromEnum) input1)
  
  putStrLn $ "\ninput2: " ++ show input2
  putStrLn $ "input2 == \"\\t  \\n\": " ++ show (input2 == "\t  \n")
  putStrLn $ "length input2: " ++ show (length input2)
  putStrLn $ "map fromEnum input2: " ++ show (map (fromEnum) input2)
  
  -- 测试字符串字面量
  let literal1 = "\t\ta\t"
  let literal2 = "\t  \n"
  putStrLn $ "\nliteral1: " ++ show literal1
  putStrLn $ "literal2: " ++ show literal2