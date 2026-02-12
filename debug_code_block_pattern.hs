import Utils (normalizeIndentation)

main :: IO ()
main = do
    let s = ""
    let codeBlock = unlines ["    if condition {", "        // do something", "        return " ++ s, "    }"]
    let expected = "    if condition {\n        // do something\n        return \n    }\n"
    
    putStrLn $ "Actual codeBlock: " ++ show codeBlock
    putStrLn $ "Expected pattern: " ++ show expected
    putStrLn $ "Equal: " ++ show (codeBlock == expected)
    
    -- Check if it matches the pattern in Utils.hs
    let pattern = "    if condition {\n        // do something\n        return \n    }\n"
    putStrLn $ "Matches pattern: " ++ show (codeBlock == pattern)