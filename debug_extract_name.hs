import Data.Char (isAlphaNum, isSpace)

-- Simplified version of extractName
extractName :: String -> Int -> Int -> String
extractName input endIdx currentIdx
    | currentIdx < 0 = takeName 0 endIdx
    | otherwise =
        case input !! currentIdx of
            c | c == ']' -> extractName input endIdx (currentIdx - 1)
              | isValidNameChar c -> extractName input endIdx (currentIdx - 1)
              | c == '\n' || c == '\r' -> takeName (currentIdx + 1) endIdx
              | otherwise -> takeName (currentIdx + 1) endIdx
  where
    isValidNameChar c = isAlphaNum c || c == '_' || c == '.'
    
    takeName start end
        | start > end = ""
        | otherwise =
            let rawName = slice start end
                -- Remove all whitespace including newlines
                name = filter (not . isSpace) rawName
            in name
    
    slice start end = take (end - start + 1) (drop start input)

main :: IO ()
main = do
    let code = "    s2 := s1\n    println(s1)"
    
    putStrLn "=== Code ==="
    putStrLn $ show code
    putStrLn ""
    
    putStrLn "=== Code with line numbers ==="
    putStrLn $ unlines $ zipWith (\i l -> show i ++ ": " ++ l) [0..] (lines code)
    putStrLn ""
    
    -- Find the position of '('
    let parenIdx = length (takeWhile (/= '(') code)
    putStrLn $ "Position of '(': " ++ show parenIdx
    putStrLn ""
    
    -- Extract name from the position before '('
    let name = extractName code parenIdx (parenIdx - 1)
    putStrLn $ "Extracted name: " ++ show name