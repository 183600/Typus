import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\f"
    putStrLn $ "s: " ++ show s
    putStrLn $ "isSpace s: " ++ show (all isSpace s)
    
    -- 测试 removeLineComments
    let withComment = s ++ "// comment"
    putStrLn $ "withComment: " ++ show withComment
    
    let processed = removeLineComments withComment
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "Expected: " ++ show s
    putStrLn $ "Test passes: " ++ show (processed == s)