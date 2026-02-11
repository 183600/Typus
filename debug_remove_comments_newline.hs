import Utils (removeComments)

main :: IO ()
main = do
    let s = "\n+"
    let withSingle = "//" ++ s
    let processed = removeComments withSingle
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "withSingle = " ++ show withSingle
    putStrLn $ "processed = " ++ show processed
    putStrLn $ "Test expects: processed = " ++ show s
    putStrLn $ "Test passes: " ++ show (processed == s)