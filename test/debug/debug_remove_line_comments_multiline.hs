import qualified Utils as U

main :: IO ()
main = do
    let lines' = ["\nV"]
    let normalizedLines = map (reverse . dropWhile (== '\n') . reverse) lines'
    let code = unlines normalizedLines
    let processed = U.removeLineComments code
    let procLines = lines processed
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "normalizedLines: " ++ show normalizedLines
    putStrLn $ "code: " ++ show code
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "procLines: " ++ show procLines
    putStrLn $ "length normalizedLines: " ++ show (length normalizedLines)
    putStrLn $ "length procLines: " ++ show (length procLines)
    putStrLn $ "Expected: length procLines === length normalizedLines"