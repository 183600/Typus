import qualified Utils as U

main :: IO ()
main = do
    let s = "a/// comment"
    let (before, after) = U.breakOn "//" s
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Before: " ++ show before
    putStrLn $ "After: " ++ show after
    putStrLn $ "Length of s: " ++ show (length s)
    putStrLn $ "Length of before: " ++ show (length before)
    
    let isTrailingSlashCase = not (null before) && 
                             length before >= 1 && 
                             not (null after) && 
                             head after == '/' &&
                             (length before + 1 < length s) &&
                             s !! (length before) == '/' &&
                             s !! (length before + 1) == '/'
    
    putStrLn $ "not (null before): " ++ show (not (null before))
    putStrLn $ "length before >= 1: " ++ show (length before >= 1)
    putStrLn $ "not (null after): " ++ show (not (null after))
    putStrLn $ "head after == '/': " ++ show (head after == '/')
    putStrLn $ "length before + 1 < length s: " ++ show (length before + 1 < length s)
    putStrLn $ "s !! (length before) == '/': " ++ show (s !! (length before) == '/')
    putStrLn $ "s !! (length before + 1) == '/': " ++ show (s !! (length before + 1) == '/')
    putStrLn $ "isTrailingSlashCase: " ++ show isTrailingSlashCase