import Utils (removeComments)

main :: IO ()
main = do
    let test = "/* first */ \" /* second */"
    putStrLn $ "Input: " ++ show test
    putStrLn $ "Output: " ++ show (removeComments test)