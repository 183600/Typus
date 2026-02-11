import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\f"
    putStrLn $ "s: " ++ show s
    putStrLn $ "isSpace s: " ++ show (isSpace (head s))
    putStrLn $ "length s == 1: " ++ show (length s == 1)
    putStrLn $ "all isSpace s: " ++ show (all isSpace s)
    
    -- 测试 removeLineComments
    let withComment = s ++ "// comment"
    putStrLn $ "withComment: " ++ show withComment
    
    -- 这里应该导入 Utils 模块来测试 removeLineComments
    -- 但为了简单起见，我们只测试 isSpace