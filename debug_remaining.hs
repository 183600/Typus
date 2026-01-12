import Utils (removeLineComments, trim)
import Data.List (isInfixOf)

-- 测试剩余的失败案例
main :: IO ()
main = do
    -- 测试案例1: "\""
    putStrLn "测试案例1: \""
    let s1 = "\""
    let stringWithComment1 = s1 ++ " // comment"
    let withoutComment1 = removeLineComments stringWithComment1
    
    putStrLn $ "原始字符串 s: " ++ show s1
    putStrLn $ "带注释字符串: " ++ show stringWithComment1
    putStrLn $ "处理后结果: " ++ show withoutComment1
    
    -- QuickCheck测试逻辑
    let quickCheckExpected1 = if "//" `isInfixOf` s1  
                             then s1
                             else if '\n' `elem` s1  
                                  then unlines (map trim (lines s1))
                                  else if s1 == "'"
                                       then s1 ++ " // comment"
                                       else trim s1
    
    putStrLn $ "QuickCheck期望: " ++ show quickCheckExpected1
    putStrLn $ "测试通过: " ++ show (withoutComment1 == quickCheckExpected1)
    putStrLn ""
    
    -- 测试案例2: "a\n"
    putStrLn "测试案例2: a\n"
    let s2 = "a\n"
    let stringWithComment2 = s2 ++ " // comment"
    let withoutComment2 = removeLineComments stringWithComment2
    
    putStrLn $ "原始字符串 s: " ++ show s2
    putStrLn $ "带注释字符串: " ++ show stringWithComment2
    putStrLn $ "处理后结果: " ++ show withoutComment2
    
    -- QuickCheck测试逻辑
    let quickCheckExpected2 = if "//" `isInfixOf` s2  
                             then s2
                             else if '\n' `elem` s2  
                                  then unlines (map trim (lines s2))
                                  else if s2 == "'"
                                       then s2 ++ " // comment"
                                       else trim s2
    
    putStrLn $ "QuickCheck期望: " ++ show quickCheckExpected2
    putStrLn $ "测试通过: " ++ show (withoutComment2 == quickCheckExpected2)
    putStrLn ""