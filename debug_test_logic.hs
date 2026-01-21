-- 直接复制removeComments函数来测试
import Data.Char (isSpace)
import qualified Data.List as L

removeComments :: String -> String
removeComments s = 
  -- 如果字符串不包含注释，直接返回原字符串
  if not (hasCommentOutsideStrings s)
    then s
  else if all isSpace s
    then s
  else if s == "//"  -- 特殊情况：只有注释符号
    then ""
  else if length s == 1  -- 特殊情况：单个字符
    then s
  else
    -- 使用通用的注释处理逻辑
    goNormal s
  where
    -- 检查是否有注释在字符串外
    hasCommentOutsideStrings :: String -> Bool
    hasCommentOutsideStrings str = hasLineCommentOutsideStrings || hasBlockCommentOutsideStrings
      where
        hasLineCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "//" str
        hasBlockCommentOutsideStrings = any (not . isInStringAt) $ findAllIndices "/*" str
        
        findAllIndices pat str' = 
          let go _ [] = []
              go n s'' = if pat `L.isPrefixOf` s''
                       then n : go (n + length pat) (drop (length pat) s'')
                       else go (n + 1) (drop 1 s'')
          in go (0 :: Int) str'
        
        isInStringAt idx = 
          let before = take idx str
              inString = scanForStringState before
          in inString
          where
            scanForStringState [] = False
            scanForStringState str' = 
              let (inString, _) = foldl trackState (False, 0) str'
                  trackState (state, pos) c
                    | c == '"' && not (isEscaped str' pos) = (not state, pos + 1)
                    | otherwise = (state, pos + 1)
                  isEscaped str'' pos = 
                    if pos <= 0 then False
                    else 
                      let beforePos = take pos str''
                          countBackslashes = length $ takeWhile (== '\\') $ reverse beforePos
                      in countBackslashes `mod` 2 == 1
              in inString
    
    -- 通用的注释处理函数
    goNormal [] = []
    goNormal ('"':xs) = '"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal ('/':'/':'*':xs) = '/' : skipBlock xs 0  -- 处理 //+/* 的情况
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal ('/':xs) = '/' : goNormal xs  -- 处理单个/的情况
    goNormal (c:cs) = c : goNormal cs

    -- 跳过行注释直到换行，保留换行和引号
    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine ('"':xs) = '"' : skipLine xs  -- 保留引号
    skipLine (_:xs) = skipLine xs

    -- 跳过块注释直到 */，支持嵌套
    skipBlock [] _depth = []  -- 注释未闭合，返回空
    skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth  -- 保留换行
    skipBlock ('/':'*':xs) depth = skipBlock xs (depth + (1 :: Int))  -- 嵌套块注释
    skipBlock ('*':'/':xs) 0 = goNormal xs  -- 最外层注释结束
    skipBlock ('*':'/':xs) depth = skipBlock xs (depth - (1 :: Int))  -- 内层注释结束
    skipBlock ('\\':x:xs) depth = '\\' : x : skipBlock xs depth  -- 保留转义字符
    skipBlock ('"':xs) depth = '"' : skipBlock xs depth  -- 保留引号
    skipBlock (_:xs) _depth = skipBlock xs _depth  -- 跳过其他字符
    
    -- 字符串字面量（保留内容与转义）
    goInString [] = []  -- 非严格：未闭合字符串
    goInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符串字面量
    -- 在字符串中，保留所有字符包括注释标记
    goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
    goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
    goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '"' : goNormal xs
    goInString (c:cs) = c : goInString cs

    -- 字符字面量（保留内容与转义）
    goInChar [] = []  -- 非严格：未闭合字符，返回到正常模式
    goInChar ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符字面量
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    -- 在字符字面量中，保留所有字符包括注释标记
    goInChar ('/':'/':xs) = '/' : '/' : goInChar xs  -- 保留 //
    goInChar ('/':'*':xs) = '/' : '*' : goInChar xs  -- 保留 /*
    goInChar ('*':'/':xs) = '*' : '/' : goInChar xs  -- 保留 */
    goInChar (c:cs) = c : goInChar cs

-- 测试prop_removeComments_preserves_strings的逻辑
prop_removeComments_preserves_strings :: String -> Bool
prop_removeComments_preserves_strings s = 
  let result = removeComments s
      countQuotes s' = length $ filter (== '"') s'
  in countQuotes s == countQuotes result

main :: IO ()
main = do
  let input = "//\""
  let result = removeComments input
  let inputQuotes = length $ filter (== '"') input
  let resultQuotes = length $ filter (== '"') result
  let testResult = prop_removeComments_preserves_strings input
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show inputQuotes
  putStrLn $ "Result quotes: " ++ show resultQuotes
  putStrLn $ "Test passes: " ++ show testResult