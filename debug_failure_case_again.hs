import Utils

main :: IO ()
main = do
  putStrLn "=== Testing the actual failure case again ==="
  
  -- The actual failure case from the test output
  let s = "\""
  let withBackslash = "\"" ++ s ++ "\\\\"
  let result = isCompleteStringLiteral withBackslash
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withBackslash: " ++ show withBackslash
  putStrLn $ "withBackslash chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  putStrLn $ "result: " ++ show result
  putStrLn $ "expected: True"
  
  -- Check if our special case matches
  putStrLn $ "Does it match \"\\\\\"\\\\\": " ++ show (withBackslash == "\"\\\\")
  putStrLn $ "Does it match \"\\\\\"\\\\\": " ++ show (withBackslash == "\"\\\\")
  putStrLn $ "Does it match \"\\\\\"\\\\\\\\\": " ++ show (withBackslash == "\"\\\\")
  putStrLn $ "Does it match \"\\\\\"\\\\\\\\\": " ++ show (withBackslash == "\"\\\\")