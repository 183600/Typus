import Utils (removeLineComments)

main :: IO ()
main = do
    let s = "\nA"
    putStrLn $ "Input s: " ++ show s
    let result = removeLineComments s
    putStrLn $ "result: " ++ show result
    putStrLn $ "lines result: " ++ show (lines result)
    putStrLn $ "length: " ++ show (length (lines result))