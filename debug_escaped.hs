import Data.List (isInfixOf)

main :: IO ()
main = do
    -- Check if the string contains a backslash
    let s = "a\""
    putStrLn $ "s = " ++ show s
    putStrLn $ "s contains backslash: " ++ show ('\\' `elem` s)
    
    -- What if we want an actual escaped quote?
    let s2 = "a\\\""
    putStrLn $ "s2 = " ++ show s2
    putStrLn $ "s2 contains backslash: " ++ show ('\\' `elem` s2)
    
    let unclosed1 = "\"" ++ s
    let unclosed2 = "\"" ++ s2
    putStrLn $ "unclosed1 = " ++ show unclosed1
    putStrLn $ "unclosed2 = " ++ show unclosed2