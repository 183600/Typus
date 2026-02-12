import Utils

main :: IO ()
main = do
    let input = "\tk"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Output: " ++ show result
    putStrLn $ "Expected: \"\\tk\" (according to test failure)"