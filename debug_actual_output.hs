import Utils as U

main :: IO ()
main = do
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = U.normalizeIndentation (unlines withMixed)
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "Expected by test: " ++ show "    "
    putStrLn $ "Actual output: " ++ show normalized