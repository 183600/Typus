import Utils (removeLineComments, trim)
import Data.List as L

-- 模拟QuickCheck测试的逻辑
main :: IO ()
main = do
    -- 测试案例1: "\na"
    putStrLn "测试案例1: \na"
    let s = "\na"
    let stringWithComment = s ++ " // comment"
    let withoutComment = removeLineComments stringWithComment
    
    putStrLn $ "原始字符串 s: " ++ show s
    putStrLn $ "带注释字符串: " ++ show stringWithComment
    putStrLn $ "处理后结果: " ++ show withoutComment
    
    -- QuickCheck测试逻辑
    let quickCheckExpected = if '\n' `elem` s  
                             then unlines (map trim (lines s))
                             else if s == "'"
                                  then s ++ " // comment"
                                  else trim s
    
    putStrLn $ "QuickCheck期望: " ++ show quickCheckExpected
    putStrLn $ "测试通过: " ++ show (withoutComment == quickCheckExpected)
    putStrLn ""
    
    -- 测试案例2: "'a"
    putStrLn "测试案例2: 'a"
    let s2 = "'a"
    let stringWithComment2 = s2 ++ " // comment"
    let withoutComment2 = removeLineComments stringWithComment2
    
    putStrLn $ "原始字符串 s: " ++ show s2
    putStrLn $ "带注释字符串: " ++ show stringWithComment2
    putStrLn $ "处理后结果: " ++ show withoutComment2
    
    -- QuickCheck测试逻辑
    let quickCheckExpected2 = if '\n' `elem` s2  
                              then unlines (map trim (lines s2))
                              else if s2 == "'"
                                   then s2 ++ " // comment"
                                   else trim s2
    
    putStrLn $ "QuickCheck期望: " ++ show quickCheckExpected2
    putStrLn $ "测试通过: " ++ show (withoutComment2 == quickCheckExpected2)
    putStrLn ""