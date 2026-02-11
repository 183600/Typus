import Data.List (isPrefixOf)

main :: IO ()
main = do
    let s = "a/"
    let withComment = s ++ "// comment"
    
    putStrLn $ "Original string: " ++ show s
    putStrLn $ "With comment: " ++ show withComment
    
    -- 测试我的逻辑
    let before = "a"
    let hasTrailingSlash = not (null before) && last before == '/'
    
    putStrLn $ "Before: " ++ show before
    putStrLn $ "hasTrailingSlash: " ++ show hasTrailingSlash
    
    let testCondition = hasTrailingSlash && length before >= 2 && (take (length before - 1) before ++ "/" ++ "//") `isPrefixOf` withComment
    
    putStrLn $ "Test condition: " ++ show testCondition
    
    -- 实际上，我们需要检查的是原始字符串是否以斜杠结尾
    let originalEndsWithSlash = not (null s) && last s == '/'
    putStrLn $ "Original ends with slash: " ++ show originalEndsWithSlash