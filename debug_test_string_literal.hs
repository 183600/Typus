import Utils

main :: IO ()
main = do
  putStrLn "Testing isCompleteStringLiteral with specific cases:"
  
  -- Test case 1: "'"
  putStrLn "\n1. Testing isCompleteStringLiteral with \"'\":"
  let input1 = "'"
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input1)
  putStrLn $ "Expected: False"
  putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral input1 == False)
  
  -- Test case 2: "\""
  putStrLn "\n2. Testing isCompleteStringLiteral with \"\\\"\":"
  let input2 = "\""
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input2)
  putStrLn $ "Expected: False"
  putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral input2 == False)
  
  -- Test case 3: "\"\""
  putStrLn "\n3. Testing isCompleteStringLiteral with \"\\\"\\\"\":"
  let input3 = "\"\""
  putStrLn $ "Input: " ++ show input3
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input3)
  putStrLn $ "Expected: False"
  putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral input3 == False)