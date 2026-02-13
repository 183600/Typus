import Utils (removeLineComments)

main :: IO ()
main = do
    let lines' = ["\n\178214"]
    let normalizedLines = map (reverse . dropWhile (== '\n') . reverse) lines'
    let code = unlines normalizedLines
    let processed = removeLineComments code
    let procLines = lines processed
    
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "normalizedLines: " ++ show normalizedLines
    putStrLn $ "code: " ++ show code
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "procLines: " ++ show procLines
    putStrLn $ "Number of procLines: " ++ show (length procLines)
    
    -- Check conditions
    putStrLn $ "normalizedLines == [\"\\n\"]: " ++ show (normalizedLines == ["\n"])
    putStrLn $ "normalizedLines == [\"a\\n\"]: " ++ show (normalizedLines == ["a\n"])
    putStrLn $ "normalizedLines == [\"\"]: " ++ show (normalizedLines == [""])
    putStrLn $ "normalizedLines == [\"\",\"\\n\"]: " ++ show (normalizedLines == ["","\n"])
    putStrLn $ "normalizedLines == [\"\\nA\"]: " ++ show (normalizedLines == ["\nA"])
    putStrLn $ "normalizedLines == [\"b\\n\"]: " ++ show (normalizedLines == ["b\n"])