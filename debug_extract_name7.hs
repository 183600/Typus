import Data.Char (isAlphaNum, isSpace)

-- Test the extractName function with debugging
extractName :: String -> Int -> Int -> Maybe String
extractName input endIdx currentIdx
    | currentIdx < 0 = takeName 0 endIdx
    | otherwise =
        let c = input !! currentIdx
        in if isValidNameChar c
            then extractName input endIdx (currentIdx - 1)
            else if isSpace c
                then takeName (currentIdx + 1) endIdx
                else if c == ']'
                    then case findMatching '[' currentIdx of
                        Nothing -> Nothing
                        Just start -> extractName input endIdx (start - 1)
                    else takeName (currentIdx + 1) endIdx
  where
    isValidNameChar c = isAlphaNum c || c == '_' || c == '.'
    
    takeName start end
        | start > end = Nothing
        | otherwise =
            let rawName = slice start end input
                -- Remove all whitespace including newlines
                name = filter (not . isSpace) rawName
            in if null name || name `elem` keywords
                  then Nothing
                  else Just name
    
    slice start end input = 
        if end >= start
            then take (end - start + 1) (drop start input)
            else take (start - end + 1) (drop end input)
    
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
    putStrLn $ unlines $ zipWith (\i l -> show i ++ ": " ++ l ++ " (" ++ show (l !! 0) ++ ")") [0..] (lines code)
    putStrLn ""
    
    -- Find the position of '('
    let parenIdx = length (takeWhile (/= '(') code)
    putStrLn $ "Position of '(': " ++ show parenIdx
    putStrLn ""
    
    -- Extract name from the position before '('
    let name = extractName code parenIdx (parenIdx - 1)
    putStrLn $ "Extracted name: " ++ show name
    
    -- Let's debug step by step
    putStrLn "\n=== Debug step by step ==="
    let startIdx = parenIdx
    let currentIdx = parenIdx - 1
    putStrLn $ "startIdx: " ++ show startIdx
    putStrLn $ "currentIdx: " ++ show currentIdx
    
    -- Let's see what happens when we call extractName with different parameters
    putStrLn "\n=== Different extractName calls ==="
    let name1 = extractName code startIdx (startIdx - 1)
    putStrLn $ "extractName code startIdx (startIdx - 1): " ++ show name1