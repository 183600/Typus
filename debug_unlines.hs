import Data.List

main :: IO ()
main = do
    let s = "A\n"
    let lined = lines s
    let rejoined = unlines lined
    putStrLn $ "Original: " ++ show s
    putStrLn $ "Lines: " ++ show lined
    putStrLn $ "Rejoined: " ++ show rejoined
    putStrLn $ "Ends with newline: " ++ show ("\n" `isSuffixOf` rejoined)