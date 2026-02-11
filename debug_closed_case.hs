import qualified Utils as U

main :: IO ()
main = do
    let closed = "\"a\"\""
    
    putStrLn $ "Debugging isProblematicUnclosedString for closed: " ++ show closed
    putStrLn $ "String characters: " ++ show (zip [0..] closed)
    putStrLn $ "String length: " ++ show (length closed)
    
    -- Check the pattern conditions
    let c = head closed
    putStrLn $ "\nPattern conditions:"
    putStrLn $ "c == '\"': " ++ show (c == '"')
    putStrLn $ "length s >= 4: " ++ show (length closed >= 4)
    putStrLn $ "s !! 0 == '\"': " ++ show (closed !! 0 == '"')
    putStrLn $ "s !! (length s - 1) == '\"': " ++ show (closed !! (length closed - 1) == '"')
    putStrLn $ "s !! (length s - 2) == '\\': " ++ show (closed !! (length closed - 2) == '\\')
    
    -- Check the new pattern
    putStrLn $ "\nNew pattern conditions:"
    putStrLn $ "c == '\"': " ++ show (c == '"')
    putStrLn $ "length s >= 3: " ++ show (length closed >= 3)
    putStrLn $ "s !! (length s - 1) == '\"': " ++ show (closed !! (length closed - 1) == '"')
    putStrLn $ "s !! (length s - 2) /= '\\': " ++ show (closed !! (length closed - 2) /= '\\')
    
    putStrLn $ "\nFinal result: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral closed)