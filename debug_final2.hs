-- 调试脚本
import Data.Char

main :: IO ()
main = do
    let s = "\""
    let sWithComment = s ++ " // comment"
    putStrLn $ "Original string: " ++ show s
    putStrLn $ "With comment: " ++ show sWithComment
    putStrLn $ "takeWhile result: " ++ show (takeWhile (/= ' ') sWithComment)
    putStrLn $ "isSpecialString: " ++ show (isSpecialString (takeWhile (/= ' ') sWithComment))
    putStrLn $ "removeLineComments result: " ++ show (removeLineComments sWithComment)
  where
    isSpecialString str = case str of
      "'" -> True   -- 单引号
      "\"" -> True  -- 双引号
      _ -> if not (null str) && (head str == '\'' || head str == '\"')
           then True  -- 以单引号或双引号开始的字符串
           else False
    
    -- 简化的 removeLineComments 函数
    removeLineComments s = 
      if null s 
        then s
        else if '\n' `elem` s
             then s  -- 简化多行处理
             else if isSpecialString (takeWhile (/= ' ') s)
                  then s
                  else s  -- 简化处理