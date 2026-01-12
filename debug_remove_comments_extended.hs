import Utils (removeLineComments)
import Data.List as L
import Data.Char

-- 模拟 QuickCheck 生成的各种边界情况
main :: IO ()
main = do
    -- 测试包含换行符的字符串
    putStrLn "测试1: 包含换行符的字符串"
    let test1 = "hello\nworld // comment"
    let result1 = removeLineComments test1
    let expected1 = "hello\nworld"
    putStrLn $ "输入: " ++ show test1
    putStrLn $ "输出: " ++ show result1
    putStrLn $ "期望: " ++ show expected1
    putStrLn $ "通过: " ++ show (result1 == expected1)
    putStrLn ""
    
    -- 测试字符串中包含引号和 //
    putStrLn "测试2: 字符串中包含引号和 //"
    let test2 = "\"hello // world\" // comment"
    let result2 = removeLineComments test2
    let expected2 = "\"hello // world\""
    putStrLn $ "输入: " ++ show test2
    putStrLn $ "输出: " ++ show result2
    putStrLn $ "期望: " ++ show expected2
    putStrLn $ "通过: " ++ show (result2 == expected2)
    putStrLn ""
    
    -- 测试转义字符
    putStrLn "测试3: 转义字符"
    let test3 = "\"hello \\\" // world\" // comment"
    let result3 = removeLineComments test3
    let expected3 = "\"hello \\\" // world\""
    putStrLn $ "输入: " ++ show test3
    putStrLn $ "输出: " ++ show result3
    putStrLn $ "期望: " ++ show expected3
    putStrLn $ "通过: " ++ show (result3 == expected3)
    putStrLn ""
    
    -- 测试字符字面量
    putStrLn "测试4: 字符字面量"
    let test4 = "'/' // comment"
    let result4 = removeLineComments test4
    let expected4 = "'/'"
    putStrLn $ "输入: " ++ show test4
    putStrLn $ "输出: " ++ show result4
    putStrLn $ "期望: " ++ show expected4
    putStrLn $ "通过: " ++ show (result4 == expected4)
    putStrLn ""
    
    -- 测试空字符串加注释
    putStrLn "测试5: 空字符串加注释"
    let test5 = "\"\" // comment"
    let result5 = removeLineComments test5
    let expected5 = "\"\""
    putStrLn $ "输入: " ++ show test5
    putStrLn $ "输出: " ++ show result5
    putStrLn $ "期望: " ++ show expected5
    putStrLn $ "通过: " ++ show (result5 == expected5)
    putStrLn ""
    
    -- 测试只有单引号的情况
    putStrLn "测试6: 只有单引号"
    let test6 = "' // comment"
    let result6 = removeLineComments test6
    let expected6 = "'"
    putStrLn $ "输入: " ++ show test6
    putStrLn $ "输出: " ++ show result6
    putStrLn $ "期望: " ++ show expected6
    putStrLn $ "通过: " ++ show (result6 == expected6)
    putStrLn ""
    
    -- 测试多个 // 的情况
    putStrLn "测试7: 多个 // 的情况"
    let test7 = "\"// //\" // comment"
    let result7 = removeLineComments test7
    let expected7 = "\"// //\""
    putStrLn $ "输入: " ++ show test7
    putStrLn $ "输出: " ++ show result7
    putStrLn $ "期望: " ++ show expected7
    putStrLn $ "通过: " ++ show (result7 == expected7)
    putStrLn ""
    
    -- 测试未闭合的字符串
    putStrLn "测试8: 未闭合的字符串"
    let test8 = "\"hello // world"
    let result8 = removeLineComments test8
    let expected8 = "\"hello // world"
    putStrLn $ "输入: " ++ show test8
    putStrLn $ "输出: " ++ show result8
    putStrLn $ "期望: " ++ show expected8
    putStrLn $ "通过: " ++ show (result8 == expected8)
    putStrLn ""
    
    -- 测试字符串中的换行符
    putStrLn "测试9: 字符串中的换行符"
    let test9 = "\"hello\n// world\" // comment"
    let result9 = removeLineComments test9
    let expected9 = "\"hello\n// world\""
    putStrLn $ "输入: " ++ show test9
    putStrLn $ "输出: " ++ show result9
    putStrLn $ "期望: " ++ show expected9
    putStrLn $ "通过: " ++ show (result9 == expected9)
    putStrLn ""