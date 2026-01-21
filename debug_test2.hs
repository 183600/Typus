import Utils

main :: IO ()
main = do
    let testInput = "\"/*"
    let result = removeComments testInput
    putStrLn $ "Input: " ++ show testInput
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Has /*: " ++ show ("/*" `isInfixOf` result)
    putStrLn $ "Has //: " ++ show ("//" `isInfixOf` result)