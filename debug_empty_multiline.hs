import Utils (normalizeIndentation)

main :: IO ()
main = do
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    let input = unlines withMixed
    putStrLn $ "input: " ++ show input
    let normalized = normalizeIndentation input
    putStrLn $ "normalized: " ++ show normalized
    let normLines = lines normalized
    putStrLn $ "normLines: " ++ show normLines
    putStrLn $ "length lines': " ++ show (length lines')
    putStrLn $ "length normLines: " ++ show (length normLines)