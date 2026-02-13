import Utils

main :: IO ()
main = do
    let lines' = ["\n"]
    let withMixed = map ("\t  " ++) lines'
    let normalized = normalizeIndentation (unlines withMixed)
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected: \"\\n\"  -- 只包含换行符的情况保持不变"