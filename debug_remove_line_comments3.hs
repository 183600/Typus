import Utils (removeLineComments)

main :: IO ()
main = do
    let test1 = "' // comment"
    putStrLn $ "Input: " ++ show test1
    putStrLn $ "Output: " ++ show (removeLineComments test1)
    putStrLn $ "Expected: " ++ show "' // comment"
    
    let test2 = "\" // comment"
    putStrLn $ "\nInput: " ++ show test2
    putStrLn $ "Output: " ++ show (removeLineComments test2)
    putStrLn $ "Expected: " ++ show "\" // comment"