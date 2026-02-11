import qualified Utils as U

main :: IO ()
main = do
    let s = "a/// comment"
    let (before, after) = U.breakOn "//" s
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "Before: " ++ show before
    putStrLn $ "After: " ++ show after
    putStrLn $ "Last char of before: " ++ show (if not (null before) then [last before] else "N/A")
    putStrLn $ "Length of before: " ++ show (length before)
    putStrLn $ "before ends with '/': " ++ show (not (null before) && last before == '/')
    
    -- 检查原始字符串中注释前的内容
    let originalS = "a/"
    let withComment = originalS ++ "// comment"
    let (origBefore, origAfter) = U.breakOn "//" withComment
    
    putStrLn "\nFor original case:"
    putStrLn $ "Original: " ++ show originalS
    putStrLn $ "With comment: " ++ show withComment
    putStrLn $ "Original before: " ++ show origBefore
    putStrLn $ "Original after: " ++ show origAfter