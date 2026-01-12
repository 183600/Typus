import Utils (removeLineComments, trim)
import Data.List as L

-- 测试多行处理
main :: IO ()
main = do
    -- 测试案例: "\na"
    putStrLn "测试: \\na"
    let s = "\na"
    let stringWithComment = s ++ " // comment"
    let result = removeLineComments stringWithComment
    
    putStrLn $ "原始字符串: " ++ show s
    putStrLn $ "带注释字符串: " ++ show stringWithComment
    putStrLn $ "处理后结果: " ++ show result
    
    -- 计算期望值
    let expected = unlines $ map trim (lines s)
    putStrLn $ "期望结果: " ++ show expected
    
    putStrLn $ "lines s: " ++ show (lines s)
    putStrLn $ "map trim (lines s): " ++ show (map trim (lines s))
    putStrLn $ "unlines result: " ++ show (unlines (map trim (lines s)))
    
    -- 检查原始字符串是否以换行符结尾
    let endsWithNewline = not (null s) && last s == '\n'
    putStrLn $ "endsWithNewline: " ++ show endsWithNewline
    
    putStrLn $ "测试通过: " ++ show (result == expected)
    putStrLn ""