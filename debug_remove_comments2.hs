import Utils

main :: IO ()
main = do
    -- Test cases for removeComments
    let test1 = "hello // comment"
    putStrLn $ "Input: " ++ show test1
    putStrLn $ "Output: " ++ show (removeComments test1)
    putStrLn ""
    
    let test2 = "hello \"// not a comment\" world // comment"
    putStrLn $ "Input: " ++ show test2
    putStrLn $ "Output: " ++ show (removeComments test2)
    putStrLn ""
    
    let test3 = "\"// not a comment\""
    putStrLn $ "Input: " ++ show test3
    putStrLn $ "Output: " ++ show (removeComments test3)
    putStrLn ""
    
    let test4 = "hello /* block comment */ world"
    putStrLn $ "Input: " ++ show test4
    putStrLn $ "Output: " ++ show (removeComments test4)
    putStrLn ""
    
    let test5 = "hello \"/* not a comment */\" world /* comment */"
    putStrLn $ "Input: " ++ show test5
    putStrLn $ "Output: " ++ show (removeComments test5)
    putStrLn ""