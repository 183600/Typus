import Utils (normalizeIndentation)

-- Test for prop_normalize_indentation_multiline_mixed with ["\n"]
main :: IO ()
main = do
    let lines' = ["\n"]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    let normalized = normalizeIndentation input
    let normLines = lines normalized
    
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Normalized lines: " ++ show normLines
    putStrLn $ "Length of input lines: " ++ show (length lines')
    putStrLn $ "Length of normalized lines: " ++ show (length normLines)