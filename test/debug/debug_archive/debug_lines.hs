main :: IO ()
main = do
    let s = "\t  \n"
    putStrLn $ "s: " ++ show s
    putStrLn $ "lines s: " ++ show (lines s)
    putStrLn $ "length (lines s): " ++ show (length (lines s))
