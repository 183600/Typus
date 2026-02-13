import qualified Utils as U

main :: IO ()
main = do
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    let result = U.normalizeIndentation input
    putStrLn $ "lines': " ++ show lines'
    putStrLn $ "withMixed: " ++ show withMixed
    putStrLn $ "input (unlines withMixed): " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"    \""