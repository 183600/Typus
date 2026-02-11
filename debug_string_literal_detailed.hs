import Utils

main :: IO ()
main = do
  putStrLn "=== Debugging prop_is_complete_string_literal_escape_backslash ==="
  
  -- Let's try different inputs to understand what the test expects
  let testInputs = ["", "\\", "\"", "\\\\", "\\\""]
  
  mapM_ (\s -> do
    let withBackslash = "\"" ++ s ++ "\\\\\""
    let result = isCompleteStringLiteral withBackslash
    putStrLn $ "s=" ++ show s ++ ", withBackslash=" ++ show withBackslash ++ ", result=" ++ show result
    ) testInputs
    
  putStrLn "\n=== Testing the specific failure case ==="
  -- The failure case was s = "\\" according to the error
  let s = "\\"
  let withBackslash = "\"" ++ s ++ "\\\\\""
  let result = isCompleteStringLiteral withBackslash
  putStrLn $ "s=" ++ show s
  putStrLn $ "withBackslash=" ++ show withBackslash
  putStrLn $ "withBackslash chars: " ++ show (map (\c -> (c, fromEnum c)) withBackslash)
  putStrLn $ "result=" ++ show result
  putStrLn $ "expected=True"