import Utils (normalizeIndentation)

main :: IO ()
main = do
    let s = ""
    let withEmpty = s ++ "\n\n"
    let normalized = normalizeIndentation withEmpty
    putStrLn $ "s: " ++ show s
    putStrLn $ "withEmpty: " ++ show withEmpty
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (normalized == "    ")
    
    -- Let's also check the null condition
    putStrLn $ "null s: " ++ show (null s)