-- 手动分析 "/* outer /* inner */a" 的处理过程

import Data.List (isInfixOf)

-- 当前有问题的实现
removeCommentsCurrent :: String -> String
removeCommentsCurrent = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = skipBlock xs
    goNormal (c:cs) = c : goNormal cs

    skipBlock = go 1
      where
        go 1 ('*':'/':xs) = goNormal xs  -- 最外层注释结束，返回正常处理
        go n ('*':'/':xs) = go (n-1) xs  -- 内层注释结束，减少嵌套层级
        go n ('/':'*':xs) = go (n+1) xs  -- 遇到新的嵌套注释，增加嵌套层级
        go n ('\n':xs) = '\n' : go n xs  -- 保留换行，保持当前嵌套层级
        go n (c:xs) = go n xs  -- 跳过其他字符，保持当前嵌套层级
        go 1 [] = []  -- 最外层注释未闭合，返回空
        go n [] = []  -- 内层注释未闭合，返回空

-- 手动跟踪 "/* outer /* inner */a" 的处理过程
main :: IO ()
main = do
    putStrLn "=== 手动分析 \"/* outer /* inner */a\" ==="
    putStrLn "字符串: /* outer /* inner */a"
    putStrLn "位置:   0123456789012345678901234"
    putStrLn "字符:   /* outer /* inner */a"
    putStrLn ""
    putStrLn "处理过程:"
    putStrLn "1. goNormal: 遇到 /*，进入 skipBlock，剩余字符串 \" outer /* inner */a\""
    putStrLn "2. skipBlock(嵌套=1): 处理 \" outer /* inner */a\""
    putStrLn "   - 跳过 ' ' (空格)"
    putStrLn "   - 跳过 'o'"
    putStrLn "   - 跳过 'u'"
    putStrLn "   - 跳过 't'"
    putStrLn "   - 跳过 'e'"
    putStrLn "   - 跳过 'r'"
    putStrLn "   - 跳过 ' '"
    putStrLn "   - 遇到 /*，嵌套增加到2，剩余字符串 \" inner */a\""
    putStrLn "3. skipBlock(嵌套=2): 处理 \" inner */a\""
    putStrLn "   - 跳过 ' '"
    putStrLn "   - 跳过 'i'"
    putStrLn "   - 跳过 'n'"
    putStrLn "   - 跳过 'n'"
    putStrLn "   - 跳过 'e'"
    putStrLn "   - 跳过 'r'"
    putStrLn "   - 遇到 */，嵌套减少到1，剩余字符串 \"a\""
    putStrLn "4. skipBlock(嵌套=1): 处理 \"a\""
    putStrLn "   - 跳过 'a'"
    putStrLn "   - 到达字符串末尾，返回 []"
    putStrLn ""
    putStrLn "问题：在第4步，'a' 被跳过了，但 'a' 实际上应该在注释之外！"
    putStrLn ""
    putStrLn "分析：注释结构是 /* outer /* inner */，当内层注释结束时，"
    putStrLn "      整个注释结构就结束了，'a' 应该被保留。"
    putStrLn ""
    putStrLn "当前实现的问题：外层注释从第一个 /* 开始，但它的结束位置不正确。"
    
    let result = removeCommentsCurrent "/* outer /* inner */a"
    putStrLn $ "\n当前结果: " ++ show result
    putStrLn $ "期望结果: \"a\""