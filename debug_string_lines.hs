import Data.List (intercalate)

main :: IO ()
main = do
    let s = "\n"
    let ls = lines s
    let result = intercalate "\n" ls
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "lines s: " ++ show ls
    putStrLn $ "intercalate \"\\n\" ls: " ++ show result
    putStrLn $ "Expected: " ++ show s
    putStrLn $ "Equal: " ++ show (result == s)