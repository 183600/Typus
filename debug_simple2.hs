-- 简单测试脚本
import Data.Char

main :: IO ()
main = do
    let s = "\""
    let sWithComment = s ++ " // comment"
    putStrLn $ "Original string: " ++ show s
    putStrLn $ "With comment: " ++ show sWithComment
    putStrLn $ "Length of s: " ++ show (length s)
    putStrLn $ "First char of s: " ++ show (if not (null s) then head s else ' ')
    putStrLn $ "takeWhile result: " ++ show (takeWhile (/= ' ') sWithComment)
    putStrLn $ "Length of takeWhile result: " ++ show (length (takeWhile (/= ' ') sWithComment))
    putStrLn $ "takeWhile result == \"\\\"\": " ++ show (takeWhile (/= ' ') sWithComment == "\"")
    putStrLn $ "takeWhile result == \"\\\"\\\"\": " ++ show (takeWhile (/= ' ') sWithComment == "\"\"")