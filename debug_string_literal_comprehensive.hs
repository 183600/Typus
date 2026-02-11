import Utils

main :: IO ()
main = do
  putStrLn "=== Testing isCompleteStringLiteral with various inputs ==="
  
  -- Test the prop_is_complete_string_literal property
  putStrLn "\n=== Testing prop_is_complete_string_literal ==="
  let testStrings = [
        "\"", -- single quote
        "\"\"", -- empty string
        "\"\\", -- quote + backslash
        "\"\\\"", -- quote + backslash + quote
        "\"\\\\", -- quote + backslash + backslash
        "\"\\\\\"", -- quote + backslash + backslash + quote
        "\"\\\\\\", -- quote + backslash + backslash + backslash
        "\"\\\\\\\"", -- quote + backslash + backslash + backslash + quote
        "\"\\\\\\\\", -- quote + backslash + backslash + backslash + backslash
        "\"\\\\\\\\\"" -- quote + backslash + backslash + backslash + backslash + quote
        ]
  
  mapM_ (\str -> do
    let result = isCompleteStringLiteral str
    putStrLn $ show str ++ " -> " ++ show result
    ) testStrings
    
  putStrLn "\n=== Testing prop_is_complete_string_literal_escape_backslash ==="
  let testInputs = ["", "\\", "\"", "\\\\", "\\\""]
  
  mapM_ (\s -> do
    let withBackslash = "\"" ++ s ++ "\\\\"
    let result = isCompleteStringLiteral withBackslash
    putStrLn $ "s=" ++ show s ++ ", withBackslash=" ++ show withBackslash ++ ", result=" ++ show result
    ) testInputs