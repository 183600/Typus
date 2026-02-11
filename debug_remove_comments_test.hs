import Utils

main :: IO ()
main = do
    let s = "b\n"
    let withSingle = "//" ++ s
    let processed = U.removeComments withSingle
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "withSingle: " ++ show withSingle
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "Expected: " ++ show "\nb"
    putStrLn $ "Match: " ++ show (processed == "\nb")