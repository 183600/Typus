import Data.Char (isSpace)

-- Test the slice function
slice :: Int -> Int -> String -> String
slice start end input = 
    if end >= start
        then take (end - start + 1) (drop start input)
        else take (start - end + 1) (drop end input)

main :: IO ()
main = do
    let code = "    s2 := s1\n    println(s1)"
    
    putStrLn "=== Code ==="
    putStrLn $ show code
    putStrLn ""
    
    putStrLn "=== Code with line numbers ==="
    putStrLn $ unlines $ zipWith (\i l -> show i ++ ": " ++ l ++ " (" ++ show (l !! 0) ++ ")") [0..] (lines code)
    putStrLn ""
    
    -- Find the position of '('
    let parenIdx = length (takeWhile (/= '(') code)
    putStrLn $ "Position of '(': " ++ show parenIdx
    putStrLn ""
    
    -- Try different slice ranges
    let slice1 = slice 22 23 code
    putStrLn $ "Slice 22-23: " ++ show slice1
    
    let slice2 = slice 20 23 code
    putStrLn $ "Slice 20-23: " ++ show slice2
    
    let slice3 = slice 18 23 code
    putStrLn $ "Slice 18-23: " ++ show slice3
    
    let slice4 = slice 16 23 code
    putStrLn $ "Slice 16-23: " ++ show slice4
    
    let slice5 = slice 14 23 code
    putStrLn $ "Slice 14-23: " ++ show slice5
    
    let slice6 = slice 12 23 code
    putStrLn $ "Slice 12-23: " ++ show slice6