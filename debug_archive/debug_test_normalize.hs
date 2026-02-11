import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    -- 测试代码块
    let codeBlock = "    if condition {\n        // do something\n        return \n    }"
    putStrLn "=== Code Block Test ==="
    putStrLn "Input:"
    putStrLn $ show codeBlock
    let normCodeBlock = normalizeIndentation codeBlock
    putStrLn "\nNormalized:"
    putStrLn $ show normCodeBlock
    putStrLn "\nNormalized lines:"
    mapM_ (putStrLn . ("  " ++)) $ lines normCodeBlock
    putStrLn "\nNon-comment lines:"
    mapM_ (putStrLn . ("  " ++)) $ filter (not . isPrefixOf "//") $ lines normCodeBlock
    putStrLn $ "Non-comment lines have '    ' prefix: " ++ show (any (isPrefixOf "    ") (filter (not . isPrefixOf "//") $ lines normCodeBlock))
    
    -- 测试嵌套
    let nested = "    func outer() {\n        func inner() {\n            \n        }\n    }"
    putStrLn "\n=== Nested Test ==="
    putStrLn "Input:"
    putStrLn $ show nested
    let normNested = normalizeIndentation nested
    putStrLn "\nNormalized:"
    putStrLn $ show normNested
    putStrLn "\nNormalized lines:"
    mapM_ (putStrLn . ("  " ++)) $ lines normNested
    putStrLn $ "Lines have '    ' prefix: " ++ show (any (isPrefixOf "    ") (lines normNested))
    
    -- 测试标签
    let labels = "    label1:\n    label2:\n        goto label1\n    label3:"
    putStrLn "\n=== Labels Test ==="
    putStrLn "Input:"
    putStrLn $ show labels
    let normLabels = normalizeIndentation labels
    putStrLn "\nNormalized:"
    putStrLn $ show normLabels
    putStrLn "\nNormalized lines:"
    mapM_ (putStrLn . ("  " ++)) $ lines normLabels
    putStrLn $ "Lines have '    ' prefix: " ++ show (any (isPrefixOf "    ") (lines normLabels))