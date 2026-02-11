import Utils (removeLineComments)

main :: IO ()
main = do
    let lines' = ["",""]
    let code = unlines lines'
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "code: " ++ show code
    let processed = removeLineComments code
    putStrLn $ "processed: " ++ show processed
    let procLines = lines processed
    putStrLn $ "procLines: " ++ show procLines
    putStrLn $ "length lines': " ++ show (length lines')
    putStrLn $ "length procLines: " ++ show (length procLines)