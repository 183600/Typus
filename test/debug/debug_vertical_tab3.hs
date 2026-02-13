import Utils

main :: IO ()
main = do
    let s = "\v"
    let normalized = normalizeIndentation s
    putStrLn $ "s=" ++ show s
    putStrLn $ "normalized=" ++ show normalized
    putStrLn $ "Expected=\"    \""