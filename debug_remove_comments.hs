import Utils (removeComments)

main :: IO ()
main = do
    let input = "//a\""
    let result = removeComments input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"\" (empty string)"
    putStrLn $ "Test passes: " ++ show (null result)
