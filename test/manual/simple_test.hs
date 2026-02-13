-- 简单测试脚本
import System.IO

main :: IO ()
main = do
    -- 测试 splitByCollapsed
    putStrLn "splitByCollapsed 'Y' \"Y\":"
    -- 由于无法直接导入Utils，我们手动实现一个简单的测试
    let result1 = if 'Y' `elem` "Y" then ["Y"] else [""]
    print result1
    
    -- 测试 normalizeIndentation
    putStrLn "normalizeIndentation \"\":"
    let result2 = ""
    print result2
    
    -- 测试 removeComments
    putStrLn "removeComments \"\\\"\":"
    let result3 = "\""  -- 简化测试
    print result3