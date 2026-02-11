import qualified Utils as U

main :: IO ()
main = do
    let s = "a/// comment"
    let processed = U.removeLineComments s
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Expected: " ++ show ("a/")
    
    -- 检查函数的各个条件
    putStrLn "\nChecking conditions:"
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "s == "\n": " ++ show (s == "\n")
    putStrLn $ "all isSpace s && s /= "\n": " ++ show (all isSpace s && s /= "\n")
    putStrLn $ "s == "//": " ++ show (s == "//")
    putStrLn $ "s == "'": " ++ show (s == "'")
    putStrLn $ "s == "/": " ++ show (s == "/")
    putStrLn $ "length s == 1: " ++ show (length s == 1)
    putStrLn $ "// `isInfixOf` s: " ++ show ("//" `isInfixOf` s)
    putStrLn $ "not ('\n' `elem` s): " ++ show (not ('\n' `elem` s))