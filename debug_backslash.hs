import Utils

main :: IO ()
main = do
  putStrLn "=== Understanding \\\\ in Haskell strings ==="
  
  -- Let's understand what \\\\ means in Haskell
  let str1 = "\\\\"
  putStrLn $ "\\\\ in Haskell = " ++ show str1 ++ " (length " ++ show (length str1) ++ ")"
  putStrLn $ "chars: " ++ show (map (\c -> (c, fromEnum c)) str1)
  
  putStrLn "\n=== Testing different combinations ==="
  let testCases = [
        ("\"", "quote"),
        ("\\", "single backslash"),
        ("\\\\", "double backslash"),
        ("\\\\\\", "triple backslash"),
        ("\\\\\\\\", "quad backslash"),
        ("\\\"", "escaped quote"),
        ("\\\\\"", "backslash + escaped quote")
        ]
  mapM_ (\(str, desc) -> do
    putStrLn $ desc ++ ": " ++ show str ++ " = " ++ show (map (\c -> (c, fromEnum c)) str)
    ) testCases