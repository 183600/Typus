import Utils (removeLineComments)

main :: IO ()
main = do
    let test1 = "\na // comment"
    putStrLn $ "Input: " ++ show test1
    putStrLn $ "Output: " ++ show (removeLineComments test1)
    putStrLn $ "Expected: " ++ show "\na"
    
    let test2 = "a // comment"
    putStrLn $ "\nInput: " ++ show test2
    putStrLn $ "Output: " ++ show (removeLineComments test2)
    putStrLn $ "Expected: " ++ show "a"
    
    let test3 = "\na\n // comment"
    putStrLn $ "\nInput: " ++ show test3
    putStrLn $ "Output: " ++ show (removeLineComments test3)
    putStrLn $ "Expected: " ++ show "\na\n "