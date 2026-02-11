import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\v"
    let withComment = s ++ "// comment"
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "With comment: " ++ show withComment
    putStrLn $ "Length of s: " ++ show (length s)
    putStrLn $ "all isSpace s: " ++ show (all isSpace s)
    putStrLn $ "length s == 1 && all isSpace s: " ++ show (length s == 1 && all isSpace s)
    
    -- 测试isSpace对各种空白字符的行为
    putStrLn "\nTesting isSpace for various whitespace characters:"
    mapM_ testChar [' ', '\t', '\n', '\r', '\f', '\v']
  where
    testChar c = putStrLn $ show c ++ " isSpace: " ++ show (isSpace c)