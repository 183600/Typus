import Utils (removeComments)

main :: IO ()
main = do
    let test1 = "/* outer /* inner */a"
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Result: " ++ show (removeComments test1)
    
    let test2 = ""
    putStrLn $ "Test 2 (empty): " ++ show test2
    putStrLn $ "Result: " ++ show (removeComments test2)
    
    let test3 = "\""
    putStrLn $ "Test 3 (quote): " ++ show test3
    putStrLn $ "Result: " ++ show (removeComments test3)