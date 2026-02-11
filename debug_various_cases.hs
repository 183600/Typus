import qualified Utils as U

main :: IO ()
main = do
    putStrLn "=== Testing various cases ==="
    
    -- 测试案例1: "\"a\\\"\""
    let test1 = "\"" ++ "a" ++ "\\\"\""
    putStrLn $ "Test1: " ++ show test1
    putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral test1)
    putStrLn $ "  Expected: True"
    
    -- 测试案例2: "\"<"
    let test2 = "\"" ++ "<"
    putStrLn $ "\nTest2: " ++ show test2
    putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral test2)
    putStrLn $ "  Expected: False (no closing quote)"
    
    -- 测试案例3: "\"."
    let test3 = "\"" ++ "."
    putStrLn $ "\nTest3: " ++ show test3
    putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral test3)
    putStrLn $ "  Expected: False (no closing quote)"
    
    -- 测试案例4: "\"E"
    let test4 = "\"" ++ "E"
    putStrLn $ "\nTest4: " ++ show test4
    putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral test4)
    putStrLn $ "  Expected: False (no closing quote)"