import Data.Char (isAlphaNum, isSpace)

-- Simplified version of extractName that matches the original
extractName :: String -> Int -> Int -> Maybe String
extractName input endIdx currentIdx
    | currentIdx < 0 = takeName 0 endIdx
    | otherwise =
        case input !! currentIdx of
            c | c == ']' -> 
                    case findMatching '[' currentIdx of
                        Nothing -> Nothing
                        Just start -> extractName input endIdx (start - 1)
              | isValidNameChar c -> extractName input endIdx (currentIdx - 1)
              | c == '\n' || c == '\r' -> takeName (currentIdx + 1) endIdx
              | otherwise -> takeName (currentIdx + 1) endIdx
  where
    isValidNameChar c = isAlphaNum c || c == '_' || c == '.'
    
    takeName start end
        | start > end = Nothing
        | otherwise =
            let rawName = slice start end
                -- Remove all whitespace including newlines
                name = filter (not . isSpace) rawName
            in if null name || name `elem` keywords
                  then Nothing
                  else Just name
    
    slice start end = take (end - start + 1) (drop start input)
    
    keywords = ["if", "for", "switch", "return", "func", "type", "var", "const", "go", "defer"]
    
    findMatching _ idx | idx < 0 = Nothing
    findMatching openChar idx = goMatch idx 0
      where
        goMatch j level
            | j < 0 = Nothing
            | otherwise =
                let ch = input !! j
                in if ch == openChar && level == 0
                    then Just j
                    else if ch == openChar
                        then goMatch (j - 1) (level + 1)
                    else if ch == ']'
                        then goMatch (j - 1) (level - 1)
                        else goMatch (j - 1) level

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