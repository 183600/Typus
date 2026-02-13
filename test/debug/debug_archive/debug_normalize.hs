import Utils (normalizeIndentation)

main :: IO ()
main = do
    let codeBlock = "    if condition {\n        // do something\n        return \n    }"
    putStrLn "Input code block:"
    putStrLn $ show codeBlock
    putStrLn "\nNormalized:"
    putStrLn $ show $ normalizeIndentation codeBlock
    putStrLn "\nNormalized lines:"
    mapM_ (putStrLn . show) $ lines $ normalizeIndentation codeBlock
    putStrLn "\nNon-comment lines:"
    mapM_ (putStrLn . show) $ filter (not . (Data.List.isPrefixOf "//")) $ lines $ normalizeIndentation codeBlock