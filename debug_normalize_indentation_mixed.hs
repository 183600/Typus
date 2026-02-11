import Utils (normalizeIndentation)

main :: IO ()
main = do
    let input = "\t  \t  a  \t  "
    putStrLn $ "Input: " ++ show input
    let output = normalizeIndentation input
    putStrLn $ "Output: " ++ show output
    putStrLn $ "Equal: " ++ show (input == output)