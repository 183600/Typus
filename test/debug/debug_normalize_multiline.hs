import qualified Utils as U

main :: IO ()
main = do
    let input = "\t  \n"
    let result = U.normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"\\n\""