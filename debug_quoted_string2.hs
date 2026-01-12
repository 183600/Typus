-- 调试脚本
import Data.Char

main :: IO ()
main = do
    let s = "'a"
    let sWithComment = s ++ " // comment"
    putStrLn $ "Original string: " ++ show s
    putStrLn $ "With comment: " ++ show sWithComment
    putStrLn $ "takeWhile result: " ++ show (takeWhile (/= ' ') sWithComment)
    putStrLn $ "isQuotedString: " ++ show (isQuotedString (takeWhile (/= ' ') sWithComment))
  where
    isQuotedString str = case str of
      "'" -> True   -- 单引号
      "\"" -> True  -- 双引号
      _ -> if not (null str) && (head str == '\'' || head str == '\"')
           then True  -- 以单引号或双引号开始的字符串
           else False