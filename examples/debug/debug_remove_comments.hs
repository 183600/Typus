import Utils (removeComments)

main :: IO ()
main = do
    let testInput = "*/"
    let result = removeComments testInput
    putStrLn $ "Input: " ++ show testInput
    putStrLn $ "Output: " ++ show result