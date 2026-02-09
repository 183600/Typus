import Utils

main :: IO ()
main = do
  putStrLn "Testing isCompleteStringLiteral with specific test cases:"
  
  -- Test case from failing test: '\
  putStrLn "\n1. Testing isCompleteStringLiteral with \"'\\\":"
  let input1 = "'\\"
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input1)
  putStrLn $ "Expected: False"
  putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral input1 == False)
  
  -- Test case: "\
  putStrLn "\n2. Testing isCompleteStringLiteral with \"\"\\\\\":"
  let input2 = "\"\\"
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input2)
  putStrLn $ "Expected: False"
  putStrLn $ "Test passes: " ++ show (isCompleteStringLiteral input2 == False)