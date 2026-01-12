import Utils (removeLineComments, trim)
import Data.List as L

-- 测试换行符处理
main :: IO ()
main = do
    -- 测试案例: "\n"
    putStrLn "测试案例: \n"
    let s = "\n"
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
    
    putStrLn $ "lines s: " ++ show (lines s)
    putStrLn $ "map trim (lines s): " ++ show (map trim (lines s))
    putStrLn $ "unlines result: " ++ show (unlines (map trim (lines s)))
    putStrLn $ "QuickCheck期望: " ++ show quickCheckExpected
    putStrLn $ "测试通过: " ++ show (withoutComment == quickCheckExpected)
    putStrLn ""