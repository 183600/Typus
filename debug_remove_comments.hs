import Utils

main :: IO ()
main = do
    -- Test cases for removeLineComments
    let test1 = "hello // comment"
    putStrLn $ "Input: " ++ show test1
    putStrLn $ "Output: " ++ show (removeLineComments test1)
    putStrLn ""
    
    let test2 = "hello \"// not a comment\" world // comment"
    putStrLn $ "Input: " ++ show test2
    putStrLn $ "Output: " ++ show (removeLineComments test2)
    putStrLn ""
    
    let test3 = "\"// not a comment\""
    putStrLn $ "Input: " ++ show test3
    putStrLn $ "Output: " ++ show (removeLineComments test3)
    putStrLn ""
    
    let test4 = "'// not a comment'"
    putStrLn $ "Input: " ++ show test4
    putStrLn $ "Output: " ++ show (removeLineComments test4)
    putStrLn ""